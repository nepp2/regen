
/// Transform structured if/let format into labelled sequence format

use crate::{bytecode, parse, symbols, env};

use bytecode::definition::*;

use env::Env;
use parse::{
  Node,
  code_segment, to_symbol,
  node_shape, NodeShape,
};
use NodeShape::*;
use symbols::Symbol;

struct LabelledBlockExpr {
  entry_seq: SeqenceId,
  exit_seq: SeqenceId,
}

/// Function builder
struct Builder<'l> {
  /// number of arguments the function takes
  args : usize,

  /// named local variables (including the arguments)
  locals : Vec<LocalVar>,

  /// local variables in scope
  scoped_locals : Vec<LocalVar>,

  /// number of 64bit registers used
  registers : usize,

  /// the instruction sequence currently being built
  cur_seq : Option<SeqenceId>,

  /// flags indicating which sequences have been finalised
  seq_completion : Vec<bool>,

  /// sequence information
  seq_info : Vec<SequenceInfo>,

  /// the operations of all the sequences
  ops : Vec<Op>,

  /// labels indicating the start and end of a labelled block expression
  label_stack : Vec<LabelledBlockExpr>,

  /// the code text
  code : &'l str,

  /// the environment containing all visible symbols
  env : &'l Env,

  /// used to store argument values without reallocating a lot
  arg_values : Vec<RegIndex>,
}

fn find_local_in_scope(b : &mut Builder, node : Node)
  -> Option<RegIndex>
{
  let c = to_symbol(b.code, node);
  b.locals.iter().rev()
    .find(|&l| l.name == c).map(|l| l.reg)
}

fn add_local(b : &mut Builder, name : Symbol, reg : RegIndex) {
  let local = LocalVar{ name, reg };
  b.locals.push(local);
  b.scoped_locals.push(local);
}

fn next_reg(b : &mut Builder) -> RegIndex {
  let r = RegIndex(b.registers);
  b.registers += 1;
  r
}

fn push_expr(b : &mut Builder, e : Expr) -> RegIndex{
  let reg = next_reg(b);
  b.ops.push(Op::Expr(reg, e));
  return reg;
}

fn create_sequence(b : &mut Builder, name : &str) -> SeqenceId {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(&name);
  loop {
    let name_unique = b.seq_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(&format!("{}_{}", name, i));
  }
  let seq_id = SeqenceId(b.seq_info.len());
  b.seq_info.push(SequenceInfo {
    name: name_candidate,
    start_op: 0, num_ops: 0,
  });
  b.seq_completion.push(false);
  seq_id
}

fn set_current_sequence(b : &mut Builder, sequence : SeqenceId) {
  // complete the current sequence (if there is one)
  complete_sequence(b);
  // check that the sequence isn't done yet
  if b.seq_completion[sequence.0] {
    panic!("this sequence has already been completed");
  }
  b.cur_seq = Some(sequence);
  b.seq_info[sequence.0].start_op = b.ops.len();
}

fn complete_sequence(b : &mut Builder) {
  if let Some(i) = b.cur_seq {
    let seq = &mut b.seq_info[i.0];
    seq.num_ops = b.ops.len() - seq.start_op;
    b.seq_completion[i.0] = true;
    b.cur_seq = None;
  }
}

fn complete_function(mut b : Builder) -> BytecodeFunction {
  complete_sequence(&mut b);
  // check all sequences are complete
  if !b.seq_completion.iter().all(|x| *x) {
    panic!("not all basic sequences were completed")
  }
  BytecodeFunction {
    sequence_info: b.seq_info,
    ops: b.ops,
    registers: b.registers,
    args: b.args,
    locals: b.locals,
  }
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> RegIndex {
  let result_reg = next_reg(b);
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_value(b, cond);
  b.ops.push(Op::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result = compile_block_expr(b, then_expr);
  if let Some(v) = then_result {
    b.ops.push(Op::Set(result_reg, v));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result = compile_block_expr(b, else_expr);
  if let Some(v) = else_result {
    b.ops.push(Op::Set(result_reg, v));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  result_reg
}

fn compile_expr_to_value(b : &mut Builder, node : Node) -> RegIndex {
  compile_expr(b, node).expect("expected value, found none")
}

fn compile_expr(b : &mut Builder, node : Node) -> Option<RegIndex> {
  match node_shape(&node, b.code) {
    // Return
    Atom("return") => {
      b.ops.push(Op::Return);
      None
    }
    // Break
    Atom("break") => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.ops.push(Op::Jump(break_to));
      None
    }
    // Repeat
    Atom("repeat") => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.ops.push(Op::Jump(loop_back_to));
      None
    }
    Atom(_) => {
      Some(atom_to_value(b, node))
    }
    // set var
    Command("set", [varname, value]) => {
      let val_reg = compile_expr_to_value(b, *value);
      let var_reg = find_local_in_scope(b, *varname).expect("no variable found");
      b.ops.push(Op::Set(var_reg, val_reg));
      None
    }
    // let
    Command("let", [var_name, value]) => {
      // evaluate the expression
      let val_reg = compile_expr_to_value(b, *value);
      // push a local variable
      let var_reg = next_reg(b);
      let name = to_symbol(b.code, *var_name);
      add_local(b, name, var_reg);
      // push a set command
      b.ops.push(Op::Set(var_reg, val_reg));
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_value(b, *cond);
      b.ops.push(Op::CJump{ cond, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_block_expr(b, *then_expr);
      b.ops.push(Op::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      let r = compile_if_else(b, *cond, *then_expr, *else_expr);
      Some(r)
    }
    // block expression
    Command("block", [body]) => {
      let entry_seq = create_sequence(b, "block_entry");
      let exit_seq = create_sequence(b, "block_exit");
      b.ops.push(Op::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledBlockExpr{ entry_seq, exit_seq }
      );
      let result = compile_block_expr(b, *body);
      b.label_stack.pop();
      b.ops.push(Op::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result
    }
  // Debug
    Command("debug", [v]) => {
      let reg = compile_expr_to_value(b, *v);
      let sym = to_symbol(b.code, *v);
      b.ops.push(Op::Debug(sym, reg));
      None
    }
    // Return
    Command("return", [v]) => {
      let reg = compile_expr_to_value(b, *v);
      b.ops.push(Op::SetReturn(reg));
      b.ops.push(Op::Return);
      None
    }
    _ => {
      let r = compile_list_expr(b, node);
      Some(r)
    }
  }
}

fn compile_block_expr(b : &mut Builder, node : Node) -> Option<RegIndex> {  
  let mut r = None;
  let num_locals = b.scoped_locals.len();
  for &c in node.children {
    r = compile_expr(b, c);
  }
  b.scoped_locals.drain(num_locals..).for_each(|_| ());
  r
}

/// Compile basic imperative language into bytecode
pub fn compile_function(env: &Env, code : &str, root : Node) -> BytecodeFunction {
  let mut b = Builder {
    args: 0,
    locals: vec![],
    scoped_locals: vec![],
    registers: 0,
    seq_completion: vec![],
    cur_seq: None,
    seq_info: vec![],
    ops: vec![],
    label_stack: vec![],
    code,
    env,
    arg_values: vec![],
  };
  let body;
  if let Command("fun", [arg_nodes, body_node]) = node_shape(&root, code) {
    body = *body_node;
    b.args = arg_nodes.children.len();
    for &arg in arg_nodes.children {
      let name = to_symbol(code, arg);
      let reg = next_reg(&mut b);
      add_local(&mut b, name, reg);
    }
  }
  else {
    panic!("expected function")
  }
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);
  let r = compile_block_expr(&mut b, body);
  if let Some(r) = r {
    b.ops.push(Op::SetReturn(r));
  }
  b.ops.push(Op::Return);
  complete_function(b)
}

fn function_call(b : &mut Builder, node : Node) -> RegIndex {
  let function_val = node.children[0];
  let args = &node.children[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = compile_expr_to_value(b, arg);
    b.arg_values.push(v);
  }
  let reg = compile_expr_to_value(b, function_val);
  for (i, value) in b.arg_values.drain(..).enumerate() {
    b.ops.push(Op::Arg{ index: i as u8, value });
  }
  let e = Expr::Invoke(reg);
  push_expr(b, e)
}

fn atom_to_value(b : &mut Builder, node : Node) -> RegIndex {
  let segment = code_segment(b.code, node);
  // boolean literals
  match segment {
    "true" => {
      let e = Expr::LiteralU64(1);
      return push_expr(b, e);
    }
    "false" => {
      let e = Expr::LiteralU64(0);
      return push_expr(b, e);
    }
    _ => (),
  }
  // integer literals
  if let Ok(v) = segment.parse::<u64>() {
    let e = Expr::LiteralU64(v);
    return push_expr(b, e);
  }
  // Look for local
  if let Some(v) = find_local_in_scope(b, node) {
    return v;
  }
  // Assume global
  let e = Expr::Def(symbols::to_symbol(segment));
  push_expr(b, e)
}

fn compile_function_call(b : &mut Builder, list : &[Node], ccall : bool) -> RegIndex {
  let function = list[0];
  let args = &list[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = compile_expr_to_value(b, arg);
    b.arg_values.push(v);
  }
  let reg = compile_expr_to_value(b, function);
  for (i, value) in b.arg_values.drain(..).enumerate() {
    b.ops.push(Op::Arg{ index: i as u8, value });
  }
  let e = {
    if ccall {
      Expr::InvokeC(reg, args.len())
    }
    else {
      Expr::Invoke(reg)
    }
  };
  return push_expr(b, e);
}

fn compile_list_expr(b : &mut Builder, node : Node) -> RegIndex {
  let head = code_segment(b.code, node.children[0]);
  let tail = &node.children[1..];
  // operator
  let op : Option<BinOp> = match head {
    "+" => Some(BinOp::Add),
    "-" => Some(BinOp::Sub),
    "*" => Some(BinOp::Mul),
    "/" => Some(BinOp::Div),
    "%" => Some(BinOp::Rem),
    "=" => Some(BinOp::Eq),
    "<" => Some(BinOp::LT),
    ">" => Some(BinOp::GT),
    "<=" => Some(BinOp::LTE),
    ">=" => Some(BinOp::GTE),
    _ => None,
  };
  if let (Some(op), [v1, v2]) = (op, tail) {
    let v1 = compile_expr_to_value(b, *v1);
    let v2 = compile_expr_to_value(b, *v2);
    let e = Expr::BinOp(op, v1, v2);
    push_expr(b, e)
  }
  else if head == "ccall" {
    compile_function_call(b, tail, true)
  }
  else {
    compile_function_call(b, node.children.as_slice(), false)
  }
}

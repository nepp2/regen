/// Compiles core language into bytecode

use crate::{bytecode, parse, symbols, env, perm_alloc};

use bytecode::{
  SeqenceId, SequenceInfo, Expr, BytecodeFunction,
  ByteWidth, NamedVar, Op, Operator, FrameVar,
};

use perm_alloc::Perm;

use symbols::to_symbol;
use env::Env;
use parse::{
  Node,
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
  locals : Vec<NamedVar>,
  
  /// local variables in scope
  scoped_locals : Vec<NamedVar>,
  
  /// registers
  registers : Vec<FrameVar>,

  /// frame size in multiple of 64bit words
  frame_bytes : usize,

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

  /// the environment containing all visible symbols
  env : &'l Env,

  /// used to store argument values without reallocating a lot
  arg_values : Vec<FrameVar>,
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum RefTag {
  Local, Register
}

use RefTag::*;

#[derive(Copy, Clone)]
struct Var {
  fv : FrameVar,
  tag : RefTag,
}

fn to_local(var : FrameVar) -> Var {
  Var { fv: var, tag: Local }
}

fn find_local_in_scope(b : &mut Builder, name : Symbol)
  -> Option<Var>
{
  b.locals.iter().rev()
    .find(|&l| l.name == name)
    .map(|l| Var { fv: l.var, tag: Local })
}

fn add_local(b : &mut Builder, name : Symbol, var : FrameVar) -> Var {
  let local = NamedVar{ name, var };
  b.locals.push(local);
  b.scoped_locals.push(local);
  Var { fv: var, tag: Local }
}

fn new_frame_var(b : &mut Builder) -> FrameVar {
  // TODO: this should take a byte width!
  let var = FrameVar{ byte_offset: b.frame_bytes, bytes: 8 };
  b.registers.push(var);
  b.frame_bytes += var.bytes;
  var
}

fn new_var(b : &mut Builder) -> Var {
  let var = new_frame_var(b);
  Var { fv: var, tag: Register }
}

fn alloca(b : &mut Builder, bytes : usize) -> Var {
  // TODO: this should be deleted
  let var = FrameVar{ byte_offset: b.frame_bytes, bytes };
  b.registers.push(var);
  b.frame_bytes += bytes;
  Var { fv: var, tag: Register }
}

fn push_expr(b : &mut Builder, e : Expr) -> Var {
  let r = new_var(b);
  b.ops.push(Op::Expr(r.fv, e));
  return r;
}

// fn build_symbol(b : &mut Builder, n : Node) -> Symbol {
//   symbols::to_symbol(b.env.st, code_segment(b.code, n))
// }

fn create_sequence(b : &mut Builder, name : &str) -> SeqenceId {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(b.env.st, &name);
  loop {
    let name_unique = b.seq_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(b.env.st, &format!("{}_{}", name, i));
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
    frame_bytes: b.frame_bytes,
    args: b.args,
    locals: b.locals,
    registers: b.registers,
  }
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> Var {
  let result_reg = new_var(b);
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_value(b, cond).fv;
  b.ops.push(Op::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result =
    compile_block_expr(b, then_expr.children());
  if let Some(v) = then_result {
    b.ops.push(Op::Set(result_reg.fv, v.fv));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result =
    compile_block_expr(b, else_expr.children());
  if let Some(v) = else_result {
    b.ops.push(Op::Set(result_reg.fv, v.fv));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  result_reg
}

fn compile_expr_to_value(b : &mut Builder, node : Node) -> Var {
  compile_expr(b, node).expect("expected value, found none")
}

fn compile_store(b : &mut Builder, byte_width : ByteWidth, pointer : Node, value : Node) {
  let pointer = compile_expr_to_value(b, pointer).fv;
  let value = compile_expr_to_value(b, value).fv;
  b.ops.push(Op::Store{ byte_width, pointer, value });
}

fn compile_expr(b : &mut Builder, node : Node) -> Option<Var> {
  match node_shape(&node) {
    // Return
    Atom("return", _) => {
      b.ops.push(Op::Return);
      None
    }
    // Break
    Atom("break", _) => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.ops.push(Op::Jump(break_to));
      None
    }
    // Repeat
    Atom("repeat", _) => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.ops.push(Op::Jump(loop_back_to));
      None
    }
    Atom(string, sym) => {
      Some(atom_to_value(b, string, sym))
    }
    // Literal
    Literal(v) => {
      let e = Expr::LiteralU64(v);
      Some(push_expr(b, e))
    }
    // quotation
    Command("#", [n]) => {
      let e = Expr::LiteralU64(Perm::to_ptr(*n) as u64);
      Some(push_expr(b, e))
    }
    // def
    Command("def", [def_name, value]) => {
      def_macro(b, *def_name, *value);
      None
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let f = compile_function(
        b.env, arg_nodes.children(), body.children());
      let f_addr = Box::into_raw(Box::new(f)) as u64;
      let r = new_var(b);
      b.ops.push(Op::Expr(r.fv, Expr::LiteralU64(f_addr)));
      Some(r)
    }
    // set var
    Command("set", [varname, value]) => {
      let val_reg = compile_expr_to_value(b, *value);
      let var_reg =
        find_local_in_scope(b, varname.as_symbol())
        .expect("no variable found");
      b.ops.push(Op::Set(var_reg.fv, val_reg.fv));
      None
    }
    // store var
    Command("store", [pointer, value]) =>
      { compile_store(b, ByteWidth::U64, *pointer, *value); None }
    Command("store_64", [pointer, value]) =>
      { compile_store(b, ByteWidth::U64, *pointer, *value); None }
    Command("store_32", [pointer, value]) =>
      { compile_store(b, ByteWidth::U32, *pointer, *value); None }
    Command("store_16", [pointer, value]) =>
      { compile_store(b, ByteWidth::U16, *pointer, *value); None }
    Command("store_8", [pointer, value]) =>
      { compile_store(b, ByteWidth::U8, *pointer, *value); None }
    // let
    Command("let", [var_name, value]) => {
      let name = var_name.as_symbol();
      // evaluate the expression
      let val = compile_expr_to_value(b, *value);
      match val.tag {
        Local => {
          add_local(b, name, val.fv);
        }
        Register => {
          let var = new_var(b).fv;
          add_local(b, name, var);
          b.ops.push(Op::Set(var, val.fv));
        }
      }
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_value(b, *cond).fv;
      b.ops.push(Op::CJump{ cond, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_block_expr(b, then_expr.children());
      b.ops.push(Op::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      let v = compile_if_else(b, *cond, *then_expr, *else_expr);
      Some(v)
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
      let result = compile_block_expr(b, body.children());
      b.label_stack.pop();
      b.ops.push(Op::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result
    }
  // Debug
    Command("debug", [v]) => {
      let r = compile_expr_to_value(b, *v);
      let sym = to_symbol(b.env.st, parse::code_segment(&*v.loc.code, *v));
      b.ops.push(Op::Debug(sym, r.fv));
      None
    }
    // Return
    Command("return", [v]) => {
      let r = compile_expr_to_value(b, *v);
      b.ops.push(Op::SetReturn(r.fv));
      b.ops.push(Op::Return);
      None
    }
    // symbol
    Command("sym", [v]) => {
      let s = v.as_symbol();
      let e = Expr::LiteralU64(s.as_u64());
      return Some(push_expr(b, e));
    }
    // symbol
    Command("alloca", [v]) => {
      let v = alloca(b, v.as_literal() as usize);
      let e = push_expr(b, Expr::UnaryOp(Operator::Ref, v.fv));
      return Some(e);
    }
    _ => {
      let v = compile_list_expr(b, node);
      Some(v)
    }
  }
}

fn compile_block_expr(b : &mut Builder, nodes : &[Node]) -> Option<Var> {  
  let mut v = None;
  let num_locals = b.scoped_locals.len();
  for &c in nodes {
    v = compile_expr(b, c);
  }
  b.scoped_locals.drain(num_locals..).for_each(|_| ());
  v
}

fn compile_function(env: &Env, args : &[Node], body : &[Node]) -> BytecodeFunction {
  let mut b = Builder {
    args: 0,
    locals: vec![],
    scoped_locals: vec![],
    registers: vec![],
    frame_bytes: 0,
    seq_completion: vec![],
    cur_seq: None,
    seq_info: vec![],
    ops: vec![],
    label_stack: vec![],
    env,
    arg_values: vec![],
  };
  b.args = args.len();
  for &arg in args {
    let name = arg.as_symbol();
    let var = new_frame_var(&mut b);
    add_local(&mut b, name, var);
  }
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);
  let v = compile_block_expr(&mut b, body);
  if let Some(v) = v {
    b.ops.push(Op::SetReturn(v.fv));
  }
  b.ops.push(Op::Return);
  complete_function(b)
}

/// Compile basic imperative language into bytecode
pub fn compile_expr_to_function(env: &Env, root : Node) -> BytecodeFunction {
  compile_function(env, &[], &[root])
}

fn function_call(b : &mut Builder, node : Node) -> Var {
  let children = node.children();
  let function_val = children[0];
  let args = &children[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = compile_expr_to_value(b, arg);
    b.arg_values.push(v.fv);
  }
  let f = compile_expr_to_value(b, function_val);
  for (i, value) in b.arg_values.drain(..).enumerate() {
    b.ops.push(Op::Arg{ index: i as u8, value });
  }
  let e = Expr::Invoke(f.fv);
  push_expr(b, e)
}

fn atom_to_value(b : &mut Builder, string : &str, sym : Symbol) -> Var {
  // boolean literals
  match string {
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
  // Look for local
  if let Some(r) = find_local_in_scope(b, sym) {
    return r;
  }
  // Assume global
  let e = Expr::Def(sym);
  push_expr(b, e)
}

fn compile_function_call(b : &mut Builder, list : &[Node], ccall : bool) -> Var {
  let function = list[0];
  let args = &list[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = compile_expr_to_value(b, arg);
    b.arg_values.push(v.fv);
  }
  let f = compile_expr_to_value(b, function);
  for (i, value) in b.arg_values.drain(..).enumerate() {
    b.ops.push(Op::Arg{ index: i as u8, value });
  }
  let e = {
    if ccall {
      Expr::InvokeC(f.fv, args.len())
    }
    else {
      Expr::Invoke(f.fv)
    }
  };
  return push_expr(b, e);
}

fn str_to_operator(s : &str) -> Option<Operator> {
  use Operator::*;
  let op = match s {
    "+" => Add,
    "-" => Sub,
    "*" => Mul,
    "/" => Div,
    "%" => Rem,
    "=" => Eq,
    "<" => LT,
    ">" => GT,
    "<=" => LTE,
    ">=" => GTE,
    "load" => Load(ByteWidth::U64),
    "load_64" => Load(ByteWidth::U64),
    "load_32" => Load(ByteWidth::U32),
    "load_16" => Load(ByteWidth::U16),
    "load_8" => Load(ByteWidth::U8),
    "Var" => Ref,
    _ => return None,
  };
  Some(op)
}

fn compile_list_expr(b : &mut Builder, node : Node) -> Var {
  match node_shape(&node) {
    Command(head, tail) => {
      let op = str_to_operator(head);
      if let (Some(op), [v1, v2]) = (op, tail) {
        let v1 = compile_expr_to_value(b, *v1);
        let v2 = compile_expr_to_value(b, *v2);
        let e = Expr::BinaryOp(op, v1.fv, v2.fv);
        return push_expr(b, e);
      }
      else if let (Some(op), [v1]) = (op, tail) {
        let v1 = compile_expr_to_value(b, *v1);
        let e = Expr::UnaryOp(op, v1.fv);
        return push_expr(b, e);
      }
      else if head == "ccall" {
        return compile_function_call(b, tail, true);
      }
    }
    _ => (),
  }
  compile_function_call(b, node.children(), false)
}

/// TODO: this is long-winded. replace it with an in-language macro!
fn def_macro(b : &mut Builder, def_name : Node, value : Node) {
  use Expr::*;
  // evaluate the expression
  let val_reg = compile_expr_to_value(b, value);
  // call env insert
  let env = {
    let env = to_symbol(b.env.st, "env");
    let v = new_frame_var(b);
    b.ops.push(Op::Expr(v, Def(env)));
    v
  };
  let def_sym = {
    let def = def_name.as_symbol().as_u64();
    let v = new_frame_var(b);
    b.ops.push(Op::Expr(v, LiteralU64(def)));
    v
  };
  let f = {
    let env_insert = to_symbol(b.env.st, "env_insert");
    let f = new_frame_var(b);
    b.ops.push(Op::Expr(f, Def(env_insert)));
    f
  };
  b.ops.push(Op::Arg{index: 0, value: env});
  b.ops.push(Op::Arg{index: 1, value: def_sym});
  b.ops.push(Op::Arg{index: 2, value: val_reg.fv});
  let v = new_frame_var(b);
  b.ops.push(Op::Expr(v, InvokeC(f, 3)));
}

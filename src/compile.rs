
/// Transform basic-block format into structured if/let format

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

struct LabelPair {
  entry_block: BlockIndex,
  exit_block: BlockIndex,
}

/// Function builder
struct Builder<'l> {
  args : usize,
  locals : Vec<(Symbol, RegIndex)>,
  registers : usize,
  cur_block : Option<BlockIndex>,
  block_completion : Vec<bool>,
  blocks : Vec<Block>,
  ops : Vec<Op>,
  label_stack : Vec<LabelPair>,

  code : &'l str,
  env : &'l Env,

  /// used to store argument values without reallocating a lot
  arg_values : Vec<RegIndex>,
}

fn find_local(b : &mut Builder, node : Node)
  -> Option<RegIndex>
{
  let c = to_symbol(b.code, node);
  b.locals.iter()
    .find(|&l| l.0 == c).map(|v| v.1)
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

fn create_basic_block(b : &mut Builder, name : &str) -> BlockIndex {
  let i = b.blocks.len();
  // TODO: do a better job of making sure that the name is unique
  let name = format!("{}_{}", name, i);
  b.blocks.push(Block {
    name: symbols::to_symbol(&name),
    start_op: 0, num_ops: 0,
  });
  b.block_completion.push(false);
  BlockIndex(i)
}

fn set_current_basic_block(b : &mut Builder, block : BlockIndex) {
  // complete the current block (if there is one)
  complete_basic_block(b);
  // check that the block isn't done yet
  if b.block_completion[block.0] {
    panic!("this block has already been completed");
  }
  b.cur_block = Some(block);
  b.blocks[block.0].start_op = b.ops.len();
}

fn complete_basic_block(b : &mut Builder) {
  if let Some(i) = b.cur_block {
    let block = &mut b.blocks[i.0];
    block.num_ops = b.ops.len() - block.start_op;
    b.block_completion[i.0] = true;
    b.cur_block = None;
  }
}

fn complete_function(mut b : Builder) -> BytecodeFunction {
  complete_basic_block(&mut b);
  // check all blocks are complete
  if !b.block_completion.iter().all(|x| *x) {
    panic!("not all basic blocks were completed")
  }
  BytecodeFunction {
    blocks: b.blocks,
    ops: b.ops,
    registers: b.registers,
    args: b.args,
    locals: b.locals,
  }
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> RegIndex {
  let result_reg = next_reg(b);
  let then_block = create_basic_block(b, "then");
  let else_block = create_basic_block(b, "else");
  let exit_block = create_basic_block(b, "exit");
  let cond = compile_expr_to_value(b, cond);
  b.ops.push(Op::CJump{ cond, then_block, else_block });
  set_current_basic_block(b, then_block);
  let then_result = compile_block(b, then_expr);
  if let Some(v) = then_result {
    b.ops.push(Op::Set(result_reg, v));
  }
  b.ops.push(Op::Jump(exit_block));
  set_current_basic_block(b, else_block);
  let else_result = compile_block(b, else_expr);
  if let Some(v) = else_result {
    b.ops.push(Op::Set(result_reg, v));
  }
  b.ops.push(Op::Jump(exit_block));
  set_current_basic_block(b, exit_block);
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
      let break_to = b.label_stack.last().unwrap().exit_block;
      b.ops.push(Op::Jump(break_to));
      None
    }
    // Repeat
    Atom("repeat") => {
      let loop_back_to = b.label_stack.last().unwrap().entry_block;
      b.ops.push(Op::Jump(loop_back_to));
      None
    }
    Atom(_) => {
      Some(atom_to_value(b, node))
    }
    // set var
    Command("set", [varname, value]) => {
      let var_reg = find_local(b, *varname).expect("no variable found");
      let val_reg = compile_expr_to_value(b, *value);
      b.ops.push(Op::Set(var_reg, val_reg));
      None
    }
    // let
    Command("let", [var_name, value]) => {
      // TODO: make the var name unique
      // TODO: handle scoping
      // push a local variable
      let var_reg = next_reg(b);
      let var_name = to_symbol(b.code, *var_name);
      b.locals.push((var_name, var_reg));
      // evaluate the expression
      let val_reg = compile_expr_to_value(b, *value);
      // push a set command
      b.ops.push(Op::Set(var_reg, val_reg));
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_block = create_basic_block(b, "then");
      let exit_block = create_basic_block(b, "exit");
      let cond = compile_expr_to_value(b, *cond);
      b.ops.push(Op::CJump{ cond, then_block, else_block: exit_block });
      set_current_basic_block(b, then_block);
      compile_block(b, *then_expr);
      b.ops.push(Op::Jump(exit_block));
      set_current_basic_block(b, exit_block);
      None
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      let r = compile_if_else(b, *cond, *then_expr, *else_expr);
      Some(r)
    }
    // block
    Command("block", [body]) => {
      let entry_block = create_basic_block(b, "loop");
      let exit_block = create_basic_block(b, "loop_exit");
      b.ops.push(Op::Jump(entry_block));
      set_current_basic_block(b, entry_block);
      b.label_stack.push(
        LabelPair{ entry_block, exit_block }
      );
      compile_block(b, *body);
      b.label_stack.pop();
      b.ops.push(Op::Jump(entry_block));
      set_current_basic_block(b, exit_block);
      panic!()
    }
  // Debug
    Command("debug", [v]) => {
      let reg = compile_expr_to_value(b, *v);
      b.ops.push(Op::Debug(reg));
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

fn compile_block(b : &mut Builder, node : Node) -> Option<RegIndex> {  
  let mut r = None;
  for &c in node.children {
    r = compile_expr(b, c);
  }
  r
}

/// Compile basic imperative language into bytecode
pub fn compile_function(env: &Env, code : &str, root : Node) -> BytecodeFunction {
  let mut b = Builder {
    args: 0,
    locals: vec![],
    registers: 0,
    block_completion: vec![],
    cur_block: None,
    blocks: vec![],
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
      b.locals.push((name, reg));
    }
  }
  else {
    panic!("expected function")
  }
  // start a block
  let entry_block = create_basic_block(&mut b, "entry");
  set_current_basic_block(&mut b, entry_block);
  let r = compile_block(&mut b, body);
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
  if let Some(v) = find_local(b, node) {
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

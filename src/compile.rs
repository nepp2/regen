
/// Transform basic-block format into structured if/let format

use crate::{bytecode, parse, symbols, env};

use bytecode::definition::*;

use env::Env;
use parse::{
  Node,
  code_segment, to_symbol,
  match_head, head_tail,
};
use symbols::Symbol;

/// Function builder
struct Builder<'l> {
  args : usize,
  locals : Vec<(Symbol, RegIndex)>,
  registers : usize,
  cur_block : Option<BlockIndex>,
  block_completion : Vec<bool>,
  blocks : Vec<Block>,
  ops : Vec<Op>,

  code : &'l str,
  env : &'l Env,

  /// used to store argument values without reallocating a lot
  arg_values : Vec<RegIndex>,
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

fn compile_expr(b : &mut Builder, node : Node) -> RegIndex {
  if node.children.len() == 0 {
    atom_to_value(b, node)
  }
  else {
    compile_list_expr(b, node, node.children.as_slice())
  }
}

fn compile_statement(b : &mut Builder, node : Node) {
  let segment = code_segment(b.code, node);
  let ht = head_tail(&node, b.code);
  // set var
  if let Some(("set", [varname, value])) = ht {
    let var_reg = find_local(b, *varname).expect("no variable found");
    let val_reg = compile_expr(b, *value);
    b.ops.push(Op::Set(var_reg, val_reg));
  }
  // let
  else if let Some(("let", [varname, value])) = ht {
    panic!()
  }
  // if then
  else if let Some(("if", [cond, then_expr])) = ht {
    panic!()
  }
  // if then else
  else if let Some(("if", [cond, then_expr, else_expr])) = ht {
    panic!()
  }
  // loop
  else if let Some(("loop", [body])) = ht {
    panic!()
  }
// Debug
  else if let Some(("debug", [v])) = ht {
    let reg = compile_expr(b, *v);
    b.ops.push(Op::Debug(reg));
  }
  // Return
  else if let Some(("return", [v])) = ht {
    let reg = compile_expr(b, *v);
    b.ops.push(Op::SetReturn(reg));
    b.ops.push(Op::Return);
  }
  else if segment == "return" {
    b.ops.push(Op::Return);
  }
  // Break
  else if segment == "break" {
    panic!()
  }
  // Assume expression
  else {
    compile_expr(b, node);
  }
}

fn compile_block(b : &mut Builder, node : Node) {
  for &statement in node.children {
    compile_statement(b, statement)
  }

  // if the last op was an expression, return its value
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
    ops : vec![],
    code,
    env,
    arg_values: vec![],
  };
  let body;
  if let Some([arg_nodes, body_node]) = match_head(&root, code, "fun") {
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
  compile_block(&mut b, body);
  complete_function(b)
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

fn function_call(b : &mut Builder, node : Node) -> RegIndex {
  let function_val = node.children[0];
  let args = &node.children[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = compile_expr(b, arg);
    b.arg_values.push(v);
  }
  let reg = compile_expr(b, function_val);
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

fn compile_list_expr(b : &mut Builder, node : Node, children : &[Node]) -> RegIndex {
  let head = code_segment(b.code, children[0]);
  // function call
  if head == "call" || head == "ccall" {
    b.arg_values.clear();
    for &arg in &children[2..] {
      let v = compile_expr(b, arg);
      b.arg_values.push(v);
    }
    let reg = compile_expr(b, children[1]);
    for (i, value) in b.arg_values.drain(..).enumerate() {
      b.ops.push(Op::Arg{ index: i as u8, value });
    }
    let e = {
      if head == "ccall" {
        let args = children[2..].len();
        Expr::InvokeC(reg, args)
      }
      else {
        Expr::Invoke(reg)
      }
    };
    return push_expr(b, e);
  }
  // operator
  let op : Option<BinOp> = match head {
    "+" => Some(BinOp::Add),
    "-" => Some(BinOp::Sub),
    "*" => Some(BinOp::Mul),
    "/" => Some(BinOp::Div),
    "%" => Some(BinOp::Rem),
    _ => None,
  };
  if let Some(op) = op {
    if let [v1, v2] = children[1..] {
      let v1 = compile_expr(b, v1);
      let v2 = compile_expr(b, v2);
      let e = Expr::BinOp(op, v1, v2);
      return push_expr(b, e);
    }
  }
  panic!("unrecognised instruction:\n   {}", code_segment(b.code, node))
}

fn find_local(b : &mut Builder, node : Node)
  -> Option<RegIndex>
{
  let c = to_symbol(b.code, node);
  b.locals.iter()
    .find(|&l| l.0 == c).map(|v| v.1)
}

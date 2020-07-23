
use crate::{parse, symbols, env};
use env::Env;
use parse::{
  AbstractSyntaxTree as AST,
  NodeIndex, node_children,
  code_segment, to_symbol,
  match_head,
};
use symbols::Symbol;

use std::fmt;

#[derive(Copy, Clone, Debug)]
pub struct BlockIndex(pub usize);

/// Register local to block, indexed from 0 upward
#[derive(Copy, Clone, Debug)]
pub struct RegIndex(pub usize);

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LiteralU64(u64),
  Add(RegIndex, RegIndex),
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
  Expr(RegIndex, Expr),
  Set(RegIndex, RegIndex),
  CJump{ cond: RegIndex, then_block: BlockIndex, else_block: BlockIndex },
  Debug(RegIndex),
  Jump(BlockIndex),
  Arg{ index: u8, value: RegIndex },
  Invoke(RegIndex),
  Return,
  //Error,
}

pub struct Block {
  pub name : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
}

pub struct Function {
  pub blocks : Vec<Block>,
  pub ops : Vec<Op>,
  pub args : Vec<Symbol>,
  pub registers : usize,
}

/// Block instructions
struct BlockInstrs<'l> {
  name : Symbol,
  instructions : &'l [NodeIndex],
}

pub fn codegen(env: &Env, ast : &AST, root : NodeIndex) -> Function {
  let body;
  let mut registers = 0;
  let mut args = vec![];  
  let mut locals = vec![];
  if let Some([arg_nodes, body_node]) = match_head(ast, root, "fun") {
    body = *body_node;
    for &arg in node_children(ast, *arg_nodes) {
      let name = to_symbol(ast, arg);
      locals.push((name, next_reg(&mut registers)));
      args.push(name);
    }
  }
  else {
    panic!("expected function")
  }
  let mut block_instrs = vec![];
  // Find all blocks (they can be out of order)
  for &node in node_children(ast, body) {
    if let Some(tail) = match_head(ast, node, "vars") {
      for &var in tail {
        let name = to_symbol(ast, var);
        locals.push((name, next_reg(&mut registers)));
      }
    }
    else if let Some(tail) = match_head(ast, node, "block") {
      let name = to_symbol(ast, tail[0]);
      let instructions = &tail[1..];
      block_instrs.push(BlockInstrs{ name, instructions });
    }
    else {
      panic!("expected block")
    }
  }
  // Generation block instructions
  let mut blocks = vec![];
  let mut ops = vec![];
  let mut b = Builder {
    ops: &mut ops,
    blocks: &block_instrs,
    locals: &mut locals,
    registers: &mut registers,
    ast: &ast,
    env,
  };
  for bi in block_instrs.as_slice() {
    let start_op = b.ops.len();
    for &n in bi.instructions {
      gen_instruction(&mut b, n);
    }
    let num_ops = b.ops.len() - start_op;
    blocks.push(Block {name: bi.name, start_op, num_ops });
  }
  Function { blocks, ops, registers, args }
}

fn to_block_id(blocks : &Vec<BlockInstrs>, s : Symbol) -> BlockIndex {
  let i = blocks.iter().position(|b| b.name == s).unwrap();
  BlockIndex(i)
}

fn next_reg(registers : &mut usize) -> RegIndex {
  let r = RegIndex(*registers);
  *registers += 1;
  r
}

fn push_expr(b : &mut Builder, e : Expr) -> RegIndex{
  let reg = next_reg(b.registers);
  b.ops.push(Op::Expr(reg, e));
  return reg;
}

fn expr_to_value(b : &mut Builder, node : NodeIndex) -> RegIndex {
  // check for operator
  if let Some([v1, v2]) = match_head(b.ast, node, "+") {
    let v1 = expr_to_value(b, *v1);
    let v2 = expr_to_value(b, *v2);
    let e = Expr::Add(v1, v2);
    return push_expr(b, e);
  }
  let segment = code_segment(b.ast, node);
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
  if let Some(v) = find_local(b.locals, b.ast, node) {
    return v;
  }
  // Assume global
  let e = Expr::Def(symbols::to_symbol(segment));
  return push_expr(b, e);
}

fn find_local(locals : &mut Vec<(Symbol, RegIndex)>, ast : &AST, node : NodeIndex)
  -> Option<RegIndex>
{
  let c = to_symbol(ast, node);
  locals.iter()
    .find(|&l| l.0 == c).map(|v| v.1)
}

struct Builder<'l> {
  ops : &'l mut Vec<Op>,
  blocks : &'l Vec<BlockInstrs<'l>>,
  locals : &'l mut Vec<(Symbol, RegIndex)>,
  registers : &'l mut usize,
  ast : &'l AST,
  env : &'l Env,
}

fn gen_instruction(b : &mut Builder, node : NodeIndex) {
  let op = {
    // set var
    if let Some([varname, value]) = match_head(b.ast, node, "set") {
      let var_reg = find_local(b.locals, b.ast, *varname).expect("no variable found");
      let val_reg = expr_to_value(b, *value);
      Op::Set(var_reg, val_reg)
    }
    // conditional jump
    else if let Some([cond, then_block, else_block]) = match_head(b.ast, node, "cjump") {
      let cond = expr_to_value(b, *cond);
      let then_block = to_block_id(b.blocks, to_symbol(b.ast, *then_block));
      let else_block = to_block_id(b.blocks, to_symbol(b.ast, *else_block));
      Op::CJump{cond, then_block, else_block}
    }
    // Jump
    else if let Some([block]) = match_head(b.ast, node, "jump") {
      let block = to_block_id(b.blocks, to_symbol(b.ast, *block));
      Op::Jump(block)
    }
    // Debug
    else if let Some([v]) = match_head(b.ast, node, "debug") {
      let reg = expr_to_value(b, *v);
      Op::Debug(reg)
    }
    // Exit
    else if code_segment(b.ast, node) == "return" {
      Op::Return
    }
    // Call
    else if let Some(tail) = match_head(b.ast, node, "invoke") {
      for (i, arg) in tail[1..].iter().enumerate() {
        let value = expr_to_value(b, *arg);
        b.ops.push(Op::Arg{ index: i as u8, value });
      }
      let reg = expr_to_value(b, tail[0]);
      Op::Invoke(reg)
    }
    else {
      panic!("unrecognised instruction:\n   {}", code_segment(b.ast, node))
    }
  };
  b.ops.push(op);
}

// #### Display implementations ####

impl fmt::Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, )?;
    for (i, b) in self.blocks.iter().enumerate() {
      writeln!(f, "Block {}: {}", i, b.name)?;
      let end = b.start_op + b.num_ops;
      for (i, op) in self.ops[b.start_op..end].iter().enumerate() {
        let i = b.start_op + i;
        write!(f, "   {}: ", i)?;
        writeln!(f, "{:?}", op)?;
      }
    }
    Ok(())
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::LiteralU64(v) =>
        write!(f, "{}", v)?,
      Expr::Def(sym) =>
        write!(f, "{}", sym)?,
      Expr::Add(a, b) =>
        write!(f, "reg[{}] + reg[{}]", a.0, b.0)?,
    }
    Ok(())
  }
}

impl fmt::Display for Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Op::Expr(reg, expr) =>
        write!(f, "reg[{}] = {}", reg.0, expr)?,
      Op::Set(a, b) =>
        write!(f, "reg[{}] := reg[{}]", a.0, b.0)?,
      Op::CJump{ cond, then_block, else_block } =>
        write!(f, "CJump reg[{}] to block[{}] else block[{}]",
          cond.0, then_block.0, else_block.0)?,
      Op::Debug(reg) =>
        write!(f, "Debug reg[{}]", reg.0)?,
      Op::Jump(block) =>
        write!(f, "Jump to block[{}]", block.0)?,
      Op::Arg{ index, value } =>
        write!(f, "Arg {} = reg[{}]", index, value.0)?,
      Op::Invoke(reg) =>
        write!(f, "Invoke reg[{}]", reg.0)?,
      Op::Return =>
        write!(f, "Return")?,
    }
    Ok(())
  }
}

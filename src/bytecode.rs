
use crate::{parse, symbols};
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
  Debug(Symbol),
  Jump(BlockIndex),
  Invoke(RegIndex),
  Exit,
  //Error,
}

pub struct Block {
  pub name : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
}

pub struct ByteCode {
  pub blocks : Vec<Block>,
  pub ops : Vec<Op>,
  pub registers : usize,
}

impl fmt::Display for ByteCode {
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

/// Block instructions
struct BlockInstrs<'l> {
  name : Symbol,
  instructions : &'l [NodeIndex],
}

pub fn codegen(ast : &AST, root : NodeIndex) -> ByteCode {
  let children = node_children(ast, root);
  let mut block_instrs = vec![];
  let mut locals = vec![];
  let mut registers = 0;
  // Find all blocks (they can be out of order)
  for &node in children {
    if let Some(tail) = match_head(ast, node, "vars") {
      for var in tail {
        let name = to_symbol(ast, *var);
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
  for b in block_instrs.as_slice() {
    let start_op = ops.len();
    for &n in b.instructions {
      gen_instruction(&mut ops, &block_instrs, &mut locals, &mut registers, ast, n);
    }
    let num_ops = ops.len() - start_op;
    blocks.push(Block {name: b.name, start_op, num_ops });
  }
  ByteCode { blocks, ops, registers }
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

fn push_expr(ops : &mut Vec<Op>, registers : &mut usize, e : Expr) -> RegIndex{
  let reg = next_reg(registers);
  ops.push(Op::Expr(reg, e));
  return reg;
}

fn expr_to_value(
  ops : &mut Vec<Op>,
  locals : &mut Vec<(Symbol, RegIndex)>,
  registers : &mut usize,
  ast : &AST,
  node : NodeIndex
) -> RegIndex {
  // check for operator
  if let Some([a, b]) = match_head(ast, node, "+") {
    let a = expr_to_value(ops, locals, registers, ast, *a);
    let b = expr_to_value(ops, locals, registers, ast, *b);
    let e = Expr::Add(a, b);
    return push_expr(ops, registers, e);
  }
  let segment = code_segment(ast, node);
  // boolean literals
  match segment {
    "true" => {
      let e = Expr::LiteralU64(1);
      return push_expr(ops, registers, e);
    }
    "false" => {
      let e = Expr::LiteralU64(0);
      return push_expr(ops, registers, e);
    }
    _ => (),
  }
  // integer literals
  if let Ok(v) = segment.parse::<u64>() {
    let e = Expr::LiteralU64(v);
    return push_expr(ops, registers, e);
  }
  // Look for local
  if let Some(v) = find_local(locals, ast, node) {
    return v;
  }
  // Assume global
  let e = Expr::Def(symbols::to_symbol(segment));
  return push_expr(ops, registers, e);
}

fn find_local(locals : &mut Vec<(Symbol, RegIndex)>, ast : &AST, node : NodeIndex)
  -> Option<RegIndex>
{
  let c = to_symbol(ast, node);
  locals.iter()
    .find(|&l| l.0 == c).map(|v| v.1)
}

fn gen_instruction(
  ops : &mut Vec<Op>,
  blocks : &Vec<BlockInstrs>,
  locals : &mut Vec<(Symbol, RegIndex)>,
  registers : &mut usize,
  ast : &AST,
  node : NodeIndex,
) {
  let op = {
    // set var
    if let Some([varname, value]) = match_head(ast, node, "set") {
      let var_reg = find_local(locals, ast, *varname).expect("no variable found");
      let val_reg = expr_to_value(ops, locals, registers, ast, *value);
      Op::Set(var_reg, val_reg)
    }
    // conditional jump
    else if let Some([cond, then_block, else_block]) = match_head(ast, node, "cjump") {
      let cond = expr_to_value(ops, locals, registers, ast, *cond);
      let then_block = to_block_id(blocks, to_symbol(ast, *then_block));
      let else_block = to_block_id(blocks, to_symbol(ast, *else_block));
      Op::CJump{cond, then_block, else_block}
    }
    // Jump
    else if let Some([block]) = match_head(ast, node, "jump") {
      let block = to_block_id(blocks, to_symbol(ast, *block));
      Op::Jump(block)
    }
    // Debug
    else if let Some([s]) = match_head(ast, node, "debug") {
      Op::Debug(to_symbol(ast, *s))
    }
    // Exit
    else if code_segment(ast, node) == "exit" {
      Op::Exit
    }
    // Call
    else if let Some([f]) = match_head(ast, node, "invoke") {
      let reg = expr_to_value(ops, locals, registers, ast, *f);
      Op::Invoke(reg)
    }
    else {
      panic!("unrecognised instruction:\n   {}", code_segment(ast, node))
    }
  };
  ops.push(op);
}

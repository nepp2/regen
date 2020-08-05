
use crate::{parse, symbols};
use symbols::Symbol;
use parse::Node;

#[derive(Copy, Clone, Debug)]
pub struct BlockIndex(pub usize);

/// Register local to block, indexed from 0 upward
#[derive(Copy, Clone, Debug)]
pub struct RegIndex(pub usize);

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
  Add, Sub, Mul, Div, Rem
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LiteralU64(u64),
  BinOp(BinOp, RegIndex, RegIndex),
  Invoke(RegIndex),
  InvokeC(RegIndex, usize),
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
  Expr(RegIndex, Expr),
  Set(RegIndex, RegIndex),
  SetReturn(RegIndex),
  CJump{ cond: RegIndex, then_block: BlockIndex, else_block: BlockIndex },
  Debug(RegIndex),
  Jump(BlockIndex),
  Arg{ index: u8, value: RegIndex },
  Return,
  //Error,
}

pub struct Block {
  pub name : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
}

pub struct BytecodeFunction {
  pub blocks : Vec<Block>,
  pub ops : Vec<Op>,
  pub args : Vec<Symbol>,
  pub registers : usize,
}

/// Block instructions
pub struct BlockInstrs<'l> {
  pub name : Symbol,
  pub instructions : &'l [Node],
}

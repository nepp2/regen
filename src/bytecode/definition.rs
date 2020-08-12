
use crate::symbols;
use symbols::Symbol;

/// ID of the sequence, which is actually its offset within
/// the function it belongs to; this ID is only unique within
/// the function.
#[derive(Copy, Clone, Debug)]
pub struct SeqenceId(pub usize);

/// Register local to stack frame, indexed from 0 upward
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
  CJump{ cond: RegIndex, then_seq: SeqenceId, else_seq: SeqenceId },
  Debug(RegIndex),
  Jump(SeqenceId),
  Arg{ index: u8, value: RegIndex },
  Return,
}

/// A sequence of instructions. Equivalent to a basic block,
/// but the name "sequence" is used to avoid confusion with
/// block expressions.
pub struct SequenceInfo {
  pub name : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
}

pub struct BytecodeFunction {
  pub sequence_info : Vec<SequenceInfo>,
  pub ops : Vec<Op>,
  pub args : usize,
  pub locals : Vec<(Symbol, RegIndex)>,
  pub registers : usize,
}

/// Bytecode data structure is defined here

use crate::symbols;
use symbols::Symbol;

/// ID of the sequence, which is actually its offset within
/// the function it belongs to; this ID is only unique within
/// the function.
#[derive(Copy, Clone, Debug)]
pub struct SequenceId(pub usize);

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct FrameVar {
  pub id : usize,
  pub byte_offset : usize,
  pub bytes : usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE,
  Load(ByteWidth),
  Ref,
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LiteralU64(u64),
  BinaryOp(Operator, FrameVar, FrameVar),
  UnaryOp(Operator, FrameVar),
  Invoke(FrameVar),
  InvokeC(FrameVar, usize),
}

#[derive(Copy, Clone, Debug)]
pub enum ByteWidth {
  U8, U16, U32, U64,
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
  Expr(FrameVar, Expr),
  Set(FrameVar, FrameVar),
  SetReturn(FrameVar),
  CJump{ cond: FrameVar, then_seq: SequenceId, else_seq: SequenceId },
  Debug(Symbol, FrameVar),
  Jump(SequenceId),
  Arg{ byte_offset: u64, value: FrameVar },
  Store{ byte_width : ByteWidth, pointer : FrameVar, value : FrameVar },
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

#[derive(Copy, Clone)]
pub struct NamedVar {
  pub name : Symbol,
  pub var : FrameVar,
}

pub struct BytecodeFunction {
  pub sequence_info : Vec<SequenceInfo>,
  pub ops : Vec<Op>,
  pub args : usize,
  pub locals : Vec<NamedVar>,
  pub registers : Vec<FrameVar>,
  pub frame_bytes : usize,
}

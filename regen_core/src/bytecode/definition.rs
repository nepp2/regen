/// Bytecode data structure is defined here

use crate::{symbols, types};
use symbols::Symbol;
use types::TypeHandle;

/// ID of the sequence, which is actually its offset within
/// the function it belongs to; this ID is only unique within
/// the function.
#[derive(Copy, Clone, Debug)]
pub struct SequenceId(pub usize);

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct FrameVar {
  pub id : usize,
  pub byte_offset : u32,
  pub bytes : u32,
}

#[derive(Copy, Clone, Debug)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE,
  Ref, Not,
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LiteralU64(u64),
  BinaryOp(Operator, FrameVar, FrameVar),
  UnaryOp(Operator, FrameVar),
  Invoke(FrameVar),
  InvokeC(FrameVar, usize),
  Load{ bytes: u64, ptr : FrameVar }
}

#[derive(Copy, Clone)]
pub enum Instr {
  Expr(FrameVar, Expr),
  Set(FrameVar, FrameVar),
  CJump{ cond: FrameVar, then_seq: SequenceId, else_seq: SequenceId },
  Debug(Symbol, FrameVar, TypeHandle),
  Jump(SequenceId),
  Arg{ byte_offset: u64, value: FrameVar },
  Store{ byte_width : u64, pointer : FrameVar, value : FrameVar },
  Return(Option<FrameVar>),
}

/// A sequence of instructions. Equivalent to a basic block,
/// but the name "sequence" is used to avoid confusion with
/// block expressions.
pub struct SequenceInfo {
  pub name : Symbol,
  pub start_instruction : usize,
  pub num_instructions : usize,
}

#[derive(Copy, Clone)]
pub struct NamedVar {
  pub name : Symbol,
  pub var : FrameVar,
}

pub struct FunctionBytecode {
  pub sequence_info : Vec<SequenceInfo>,
  pub instrs : Vec<Instr>,
  pub args : usize,
  pub locals : Vec<NamedVar>,
  pub registers : Vec<FrameVar>,
  pub frame_bytes : u64,
}

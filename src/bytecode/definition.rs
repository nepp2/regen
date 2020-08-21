
use crate::symbols;
use symbols::Symbol;

/// ID of the sequence, which is actually its offset within
/// the function it belongs to; this ID is only unique within
/// the function.
#[derive(Copy, Clone, Debug)]
pub struct SeqenceId(pub usize);

/// Identifies a register that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct Register {
  pub word_offset : usize
}

#[derive(Copy, Clone, Debug)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE,
  Load(Alignment),
  Ref,
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LiteralU64(u64),
  BinaryOp(Operator, Register, Register),
  UnaryOp(Operator, Register),
  Invoke(Register),
  InvokeC(Register, usize),
}

#[derive(Copy, Clone, Debug)]
pub enum Alignment {
  U8, U16, U32, U64,
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
  Expr(Register, Expr),
  Set(Register, Register),
  SetReturn(Register),
  CJump{ cond: Register, then_seq: SeqenceId, else_seq: SeqenceId },
  Debug(Symbol, Register),
  Jump(SeqenceId),
  Arg{ index: u8, value: Register },
  Store{ alignment : Alignment, pointer : Register, value : Register },
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
pub struct LocalVar {
  pub name : Symbol,
  pub reg : Register,
}

pub struct BytecodeFunction {
  pub sequence_info : Vec<SequenceInfo>,
  pub ops : Vec<Op>,
  pub args : usize,
  pub locals : Vec<LocalVar>,
  pub frame_words : usize,
}

/// Bytecode data structure is defined here

use crate::{symbols, types, perm_alloc, parse};
use symbols::Symbol;
use parse::Node;
use types::TypeHandle;
use perm_alloc::PermSlice;

/// ID of the sequence, which is actually its offset within
/// the function it belongs to; this ID is only unique within
/// the function.
#[derive(Copy, Clone, Debug)]
pub struct SequenceId(pub usize);

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone)]
pub struct LocalInfo {
  pub id : LocalId,
  pub name : Option<Symbol>,
  pub byte_offset : u64,
  pub t : TypeHandle,
}

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone)]
pub struct RegisterInfo {
  pub id : RegId,
  pub byte_offset : u64,
  pub t : TypeHandle,
}

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct LocalId {
  pub id : usize,
}

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct RegId {
  pub id : usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE, Not,
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Def(Symbol),
  LocalId(LocalId),
  LiteralU64(u64),
  BinaryOp(Operator, RegId, RegId),
  UnaryOp(Operator, RegId),
  Invoke(RegId, PermSlice<RegId>),
  InvokeC(RegId, PermSlice<RegId>),
  Load(RegId),
}

#[derive(Copy, Clone)]
pub enum Instr {
  Expr(RegId, Expr),
  CJump{ cond: RegId, then_seq: SequenceId, else_seq: SequenceId },
  Debug(Node, RegId, TypeHandle),
  Jump(SequenceId),
  Store{ pointer : RegId, value : RegId },
  Return(Option<RegId>),
}

/// A sequence of instructions. Equivalent to a basic block,
/// but the name "sequence" is used to avoid confusion with
/// block expressions.
pub struct SequenceInfo {
  pub name : Symbol,
  pub start_instruction : usize,
  pub num_instructions : usize,
}

pub struct FunctionBytecode {
  pub sequence_info : Vec<SequenceInfo>,
  pub instrs : Vec<Instr>,
  pub args : usize,
  pub locals : Vec<LocalInfo>,
  pub registers : Vec<RegisterInfo>,
  pub frame_bytes : u64,
}

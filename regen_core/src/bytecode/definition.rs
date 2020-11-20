/// Bytecode data structure is defined here

use crate::{symbols, types, perm_alloc, parse};
use symbols::Symbol;
use parse::Node;
use types::TypeHandle;
use perm_alloc::{PermSlice, Perm};

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone)]
pub struct LocalInfo {
  pub index : usize,
  pub name : Option<Symbol>,
  pub t : TypeHandle,
  pub mutable : bool,
  pub byte_offset : u64,
}

/// Identifies a storage location that is local to a stack frame
pub type LocalHandle = Perm<LocalInfo>;

pub type SequenceHandle = Perm<SequenceInfo>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE, Not,
}

#[derive(Copy, Clone)]
pub enum Expr {
  Def(Symbol),
  LocalAddr(LocalHandle),
  Init(PermSlice<LocalHandle>),
  ZeroInit,
  Array(PermSlice<LocalHandle>),
  FieldIndex { struct_addr : LocalHandle, index : u64 },
  LiteralU64(u64),
  Literal(TypeHandle, *const ()),
  BinaryOp(Operator, LocalHandle, LocalHandle),
  UnaryOp(Operator, LocalHandle),
  Invoke(LocalHandle, PermSlice<LocalHandle>),
  InvokeC(LocalHandle, PermSlice<LocalHandle>),
  Load(LocalHandle),
  PtrOffset { ptr: LocalHandle, offset: LocalHandle },
  Cast(LocalHandle),
}

#[derive(Copy, Clone)]
pub enum Instr {
  Expr(LocalHandle, Expr),
  CJump{ cond: LocalHandle, then_seq: SequenceHandle, else_seq: SequenceHandle },
  Debug(Node, LocalHandle, TypeHandle),
  Jump(SequenceHandle),
  Store{ pointer : LocalHandle, value : LocalHandle },
  Return(Option<LocalHandle>),
}

/// A sequence of instructions. Equivalent to a basic block,
/// but the name "sequence" is used to avoid confusion with
/// block expressions.
#[derive(Copy, Clone)]
pub struct SequenceInfo {
  pub index : usize,
  pub name : Symbol,
  pub start_instruction : usize,
  pub num_instructions : usize,
}

pub struct FunctionBytecode {
  pub sequence_info : Vec<SequenceHandle>,
  pub instrs : Vec<Instr>,
  pub args : usize,
  pub locals : Vec<LocalHandle>,
  pub frame_bytes : u64,
}

/// Bytecode data structure is defined here

use crate::{symbols, types, perm_alloc, sexp};
use symbols::Symbol;
use sexp::{SrcLocation};
use types::TypeHandle;
use perm_alloc::{SlicePtr, Ptr};

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
pub type LocalHandle = Ptr<LocalInfo>;

pub type SequenceHandle = Ptr<SequenceInfo>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
  // arithmetic
  Add, Sub, Mul, Div, Rem,
  // comparison
  Eq, NEq, LT, GT, LTE, GTE,
  // boolean
  Not,
  // bitwise
  BitwiseNot, BitwiseAnd, BitwiseOr,
}

#[derive(Copy, Clone)]
pub enum InstrExpr {
  Def(Symbol),
  LocalAddr(LocalHandle),
  Init(SlicePtr<LocalHandle>),
  ZeroInit,
  Array(SlicePtr<LocalHandle>),
  FieldIndex { struct_addr : LocalHandle, index : u64 },
  LiteralI64(i64),
  Literal(TypeHandle, *const ()),
  BinaryOp(Operator, LocalHandle, LocalHandle),
  UnaryOp(Operator, LocalHandle),
  Invoke(LocalHandle, SlicePtr<LocalHandle>),
  InvokeC(LocalHandle, SlicePtr<LocalHandle>),
  Load(LocalHandle),
  PtrOffset { ptr: LocalHandle, offset: LocalHandle },
  Cast(LocalHandle),
}

#[derive(Copy, Clone)]
pub enum Instr {
  Expr(LocalHandle, InstrExpr),
  CJump{ cond: LocalHandle, then_seq: SequenceHandle, else_seq: SequenceHandle },
  Debug(SrcLocation, LocalHandle, TypeHandle),
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

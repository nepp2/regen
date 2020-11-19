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
  pub t : TypeHandle,
  pub mutable : bool,
  pub byte_offset : u64,
}

/// Identifies a storage location that is local to a stack frame
#[derive(Copy, Clone, Debug)]
pub struct LocalId {
  pub id : usize,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
  Add, Sub, Mul, Div, Rem, Eq, LT, GT, LTE, GTE, Not,
}

#[derive(Copy, Clone)]
pub enum Expr {
  Def(Symbol),
  LocalAddr(LocalId),
  Init(PermSlice<LocalId>),
  Array(PermSlice<LocalId>),
  FieldIndex { struct_addr : LocalId, index : u64 },
  LiteralU64(u64),
  Literal(TypeHandle, *const ()),
  BinaryOp(Operator, LocalId, LocalId),
  UnaryOp(Operator, LocalId),
  Invoke(LocalId, PermSlice<LocalId>),
  InvokeC(LocalId, PermSlice<LocalId>),
  Load(LocalId),
  PtrOffset { ptr: LocalId, offset: LocalId },
  Cast(LocalId),
}

#[derive(Copy, Clone)]
pub enum Instr {
  Expr(LocalId, Expr),
  CJump{ cond: LocalId, then_seq: SequenceId, else_seq: SequenceId },
  Debug(Node, LocalId, TypeHandle),
  Jump(SequenceId),
  Store{ pointer : LocalId, value : LocalId },
  Return(Option<LocalId>),
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
  pub frame_bytes : u64,
}

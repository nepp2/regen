
use crate::symbols::Symbol;

pub type Type = *const TypeInfo;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeInfo {
  pub size : u64,
  pub id : Symbol,
  pub kind : Symbol,
  pub info : *const (),
}


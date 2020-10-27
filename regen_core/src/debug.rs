
use std::fmt;
use crate::types::{
  TypeHandle, Kind, type_as_primitive, Primitive
};

pub struct DebugDisplay { p : *const (), t : TypeHandle }

pub fn display(p : *const (), t : TypeHandle) -> DebugDisplay {
  DebugDisplay { p, t }
}

impl fmt::Display for DebugDisplay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.t.kind {
      Kind::Primitive => {
        match type_as_primitive(&self.t).unwrap() {
          Primitive::U64 =>
            write!(f, "{}", unsafe { *(self.p as *const u64) }),
          Primitive::U32 =>
            write!(f, "{}", unsafe { *(self.p as *const u32) }),
          Primitive::U16 =>
            write!(f, "{}", unsafe { *(self.p as *const u16) }),
          Primitive::U8 =>
            write!(f, "{}", unsafe { *(self.p as *const u8) }),
          Primitive::Void =>
            write!(f, "void"),
        }
      }
      Kind::Tuple => {
        write!(f, "tuple")
      }
      Kind::Function => {
        write!(f, "function")
      }
      Kind::Pointer => {
        write!(f, "pointer")
      }
      Kind::Array => {
        write!(f, "array")
      }
      Kind::Macro => {
        write!(f, "macro")
      }
      Kind::Type => {
        let v = unsafe{ *(self.p as *const TypeHandle) };
        write!(f, "{}", v)
      }
    }
  }
}

use std::fmt;
use crate::{types, parse};

use types::{
  TypeHandle, Kind, Primitive
};
use parse::{Expr, SrcLocation};

pub struct DebugDisplay { p : *const (), t : TypeHandle }

pub fn print_command(loc : SrcLocation, d : DebugDisplay) {
  use ansi_term::{Colour::{Cyan, Yellow, RGB}};
  println!("{}{}{}{}",
    Yellow.paint("printing expr ("),
    Yellow.paint(format!("{}", loc.src_snippet())),
    Yellow.paint("): "),
    Cyan.paint(format!("{}", d)),
  );
  println!("  {}", RGB(130, 130, 0).paint(format!("({})", loc)));
}

pub fn display(p : *const (), t : TypeHandle) -> DebugDisplay {
  DebugDisplay { p, t }
}

impl fmt::Display for DebugDisplay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.t.kind {
      Kind::Primitive => {
        match types::type_as_primitive(self.t).unwrap() {
          Primitive::I64 =>
            write!(f, "{}", unsafe { *(self.p as *const i64) }),
          Primitive::I32 =>
            write!(f, "{}", unsafe { *(self.p as *const i32) }),
          Primitive::U64 =>
            write!(f, "{}", unsafe { *(self.p as *const u64) }),
          Primitive::U32 =>
            write!(f, "{}", unsafe { *(self.p as *const u32) }),
          Primitive::U16 =>
            write!(f, "{}", unsafe { *(self.p as *const u16) }),
          Primitive::U8 =>
            write!(f, "{}", unsafe { *(self.p as *const u8) }),
          Primitive::Bool =>
            write!(f, "{}", unsafe { *(self.p as *const u8) } != 0),
          Primitive::Void =>
            write!(f, "void"),
        }
      }
      Kind::Struct => {
        write!(f, "struct")
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
      Kind::Named => {
        let i = types::type_as_named(self.t).unwrap();
        if i.name.as_str() == "expr" {
          let e = unsafe { *(self.p as *const Expr) };
          write!(f, "{}", e)
        }
        else {
          write!(f, "{}", i.name)
        }
      }
      Kind::Type => {
        let v = unsafe{ *(self.p as *const TypeHandle) };
        write!(f, "{}", v)
      }
      Kind::Poly => {
        let i = types::type_as_poly(self.t).unwrap();
        write!(f, "poly({}, {})", display(self.p, i.t), i.param)
      }
    }
  }
}

/// Defines the an extensible Type data structure to be used by the
/// compiler & interpreter.

use crate::perm_alloc::{PermSlice, perm_slice};
use crate::symbols;
use symbols::{Symbol, to_symbol, SymbolTable};

#[derive(Copy, Clone)]
pub struct Type(*const TypeInfo);

impl Type {
  pub fn get(&self) -> &TypeInfo {
    unsafe { &*self.0 }
  }

  pub fn as_u64(self) -> u64 {
    self.0 as u64
  }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeInfo {
  pub size : u64,
  pub id : Symbol,
  pub kind : Symbol,
  pub info : *const (),
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Field {
  pub t : Type,
  pub offset : u64,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct StructInfo {
  pub fields : PermSlice<Field>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct PointerInfo {
  pub points_to : Type,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct FunctionInfo {
  pub args : PermSlice<Type>,
  pub returns : Type,
}

pub struct CoreTypes {
  pub type_tag : Type,
  pub u64_tag : Type,
  pub u32_tag : Type,
  pub u16_tag : Type,
  pub u8_tag : Type,
  pub void_ptr_tag : Type,
  pub macro_tag : Type,

  pub function_kind : Symbol,

  pub core_types : Vec<Type>,
}

impl CoreTypes {
  pub fn as_function(&self, t : Type) -> &FunctionInfo {
    let t = t.get();
    if t.kind != self.function_kind {
      panic!("expected function type");
    }
    unsafe { &*(t.info as *const FunctionInfo) }
  }
}

fn round_up_multiple(v : u64, multiple_of : u64) -> u64 {
  ((v / multiple_of) + ((v % multiple_of > 0) as u64)) * multiple_of
}

fn round_up_power_of_2(v : u64) -> u64 {
  let mut power = 1;
  while power < v { power *= 2; }
  power
}

fn struct_offsets(field_types : &[Type]) -> (Vec<Field>, u64) {
  let mut fields = vec![];
  let mut offset = 0;
  for &t in field_types {
    let field_size = round_up_power_of_2(t.get().size);
    offset = round_up_multiple(offset, std::cmp::min(field_size, 8));
    fields.push(Field { t, offset });
    offset += field_size;
  }
  let size = round_up_multiple(offset, round_up_power_of_2(std::cmp::min(offset, 8)));
  (fields, size)
}

fn new_type(st : SymbolTable, name : &str, kind : &str, size : u64, info : *const ()) -> Type {
  let id = to_symbol(st, name);
  let kind = to_symbol(st, kind);
  let info = TypeInfo { size, id, kind, info };
  Type(Box::into_raw(Box::new(info)))
}

fn struct_type(st : SymbolTable, name : &str, fields : &[Type]) -> Type {
  let (fields, size) = struct_offsets(fields);
  let info =
    Box::into_raw(Box::new(
      StructInfo { fields: perm_slice(&fields)}))
    as *const ();
  new_type(st, name, "struct", size, info)
}

fn function_type(st : SymbolTable, args : &[Type], returns : Type) -> Type {
  let info =
    Box::into_raw(Box::new(
      FunctionInfo { args: perm_slice(args), returns }))
    as *const ();
  new_type(st, name, "function", 8, info)
}

fn primitive(st : SymbolTable, name : &str, size : u64) -> Type {
  new_type(st, name, "primitive", size, std::ptr::null())
}

pub fn core_types(st : SymbolTable) -> CoreTypes {

  let u64_tag = primitive(st, "u64", 8);
  let u32_tag = primitive(st, "u32", 4);
  let u16_tag = primitive(st, "u16", 2);
  let u8_tag = primitive(st, "u8", 1);
  let symbol_tag = primitive(st, "symbol", 8);
  let void_ptr_tag = primitive(st, "void_ptr", 8);

  let macro_tag = primitive(st, "macro_flag", 8);

  let function_kind = to_symbol(st, "function");

  let type_tag = struct_type(st, "type", &[
    u64_tag,
    symbol_tag,
    symbol_tag,
    void_ptr_tag
  ]);

  CoreTypes {
    type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_ptr_tag, macro_tag,
    function_kind,
    core_types:
     vec![type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_ptr_tag, macro_tag],
  }
}

#[test]
fn test_types() {
  let st = symbols::symbol_table();
  let c = core_types(st);

  let a =
    struct_type(st, "a",
      &[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]);
  let b =
    struct_type(st, "b",
      &[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]);

  if a.get().size != 24 { panic!("struct A alignment incorrect") }
  if b.get().size != 16 { panic!("struct B alignment incorrect") }
}

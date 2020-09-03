/// Defines the an extensible Type data structure to be used by the
/// compiler & interpreter.

use crate::perm_alloc::{PermSlice, perm_slice};
use crate::symbols;
use symbols::{Symbol, to_symbol, SymbolTable};

#[derive(Copy, Clone)]
#[repr(C)]
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
  pub name : Symbol,
  pub kind : Symbol,
  pub kind_info : *const (),
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
  pub primitive_kind : Symbol,
  pub struct_kind : Symbol,
  pub function_kind : Symbol,

  pub structural_type_marker : Symbol,

  pub type_tag : Type,
  pub u64_tag : Type,
  pub u32_tag : Type,
  pub u16_tag : Type,
  pub u8_tag : Type,
  pub void_tag : Type,
  pub void_ptr_tag : Type,
  pub macro_tag : Type,

  pub core_types : Vec<Type>,
}

impl CoreTypes {
  pub fn as_function(&self, t : Type) -> &FunctionInfo {
    let info = t.get();
    if info.kind != self.function_kind {
      panic!("expected function type, found {}", t.as_u64());
    }
    unsafe { &*(info.kind_info as *const FunctionInfo) }
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

fn new_type(name : Symbol, kind : Symbol, size : u64, info : *const ()) -> Type {
  let info = TypeInfo { size, name, kind, kind_info: info };
  Type(Box::into_raw(Box::new(info)))
}

fn new_simple_type(st : SymbolTable, name : &str, kind : Symbol, size : u64) -> Type {
  let name = to_symbol(st, name);
  new_type(name, kind, size, std::ptr::null())
}

pub fn composite_type(name : Symbol, kind : Symbol, fields : &[Type]) -> Type {
  let (fields, size) = struct_offsets(fields);
  let info =
    Box::into_raw(Box::new(
      StructInfo { fields: perm_slice(&fields)}))
    as *const ();
  new_type(name, kind, size, info)
}

pub fn struct_type(c : &CoreTypes, name : Symbol, fields : &[Type]) -> Type {
  composite_type(name, c.struct_kind, fields)
}

pub fn function_type(c : &CoreTypes, args : &[Type], returns : Type) -> Type {
  let info =
    Box::into_raw(Box::new(
      FunctionInfo { args: perm_slice(args), returns }))
    as *const ();
  new_type(c.structural_type_marker, c.function_kind, 8, info)
}

pub fn core_types(st : SymbolTable) -> CoreTypes {

  let primitive_kind = to_symbol(st, "primitive");
  let struct_kind = to_symbol(st, "struct");
  let function_kind = to_symbol(st, "function");

  let structural_type_marker = to_symbol(st, "#structural");

  let u64_tag = new_simple_type(st, "u64", primitive_kind, 8);
  let u32_tag = new_simple_type(st, "u32", primitive_kind, 4);
  let u16_tag = new_simple_type(st, "u16", primitive_kind, 2);
  let u8_tag = new_simple_type(st, "u8", primitive_kind, 1);
  let void_tag = new_simple_type(st, "void", primitive_kind, 0);
  let symbol_tag = new_simple_type(st, "symbol", primitive_kind, 8);
  let void_ptr_tag = new_simple_type(st, "void_ptr", primitive_kind, 8);
  let macro_tag = new_simple_type(st, "macro_flag", primitive_kind, 8);

  let type_tag = composite_type(to_symbol(st, "type"), struct_kind, &[
    u64_tag,
    symbol_tag,
    symbol_tag,
    void_ptr_tag
  ]);

  CoreTypes {
    primitive_kind, struct_kind, function_kind,
    structural_type_marker,
    type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_tag, void_ptr_tag, macro_tag,
    core_types:
     vec![type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_ptr_tag, macro_tag],
  }
}

#[test]
fn test_types() {
  let st = symbols::symbol_table();
  let c = core_types(st);

  let a =
    struct_type(&c, to_symbol(st, "a"),
      &[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]);
  let b =
    struct_type(&c, to_symbol(st, "b"),
      &[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]);

  if a.get().size != 24 { panic!("struct A alignment incorrect") }
  if b.get().size != 16 { panic!("struct B alignment incorrect") }
}

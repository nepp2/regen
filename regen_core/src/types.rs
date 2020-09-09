/// Defines the an extensible Type data structure to be used by the
/// compiler & interpreter.

use crate::perm_alloc::{PermSlice, perm_slice};
use crate::symbols;
use symbols::{Symbol, to_symbol, SymbolTable};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeHandle(*const Type);

impl TypeHandle {
  // TODO: replace this with something more efficient
  pub fn alloc_type(t : Type) -> TypeHandle {
    TypeHandle(Box::into_raw(Box::new(t)))
  }

  pub fn get(&self) -> &Type {
    unsafe { &*self.0 }
  }

  pub fn as_u64(self) -> u64 {
    self.0 as u64
  }

  pub fn from_u64(v : u64) -> Self {
    TypeHandle(v as *const Type)
  }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Type {
  pub size_of : u64,
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
pub struct TupleInfo {
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
  pub c_function : bool,
}

pub struct CoreTypes {
  pub primitive_kind : Symbol,
  pub tuple_kind : Symbol,
  pub function_kind : Symbol,
  pub pointer_kind : Symbol,
  pub array_kind : Symbol,

  pub type_tag : Type,
  pub u64_tag : Type,
  pub u32_tag : Type,
  pub u16_tag : Type,
  pub u8_tag : Type,
  pub void_tag : Type,
  pub slice_tag : Type,

  pub core_types : Vec<(Symbol, Type)>,
}

impl CoreTypes {
  pub fn as_function(&self, t : &Type) -> Option<&FunctionInfo> {
    if t.kind != self.function_kind {
      return None;
    }
    Some(unsafe { &*(t.kind_info as *const FunctionInfo) })
  }
}

pub fn round_up_multiple(v : u64, multiple_of : u64) -> u64 {
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
    let field_size = round_up_power_of_2(t.size_of);
    offset = round_up_multiple(offset, std::cmp::min(field_size, 8));
    fields.push(Field { t, offset });
    offset += field_size;
  }
  let size = round_up_multiple(offset, round_up_power_of_2(std::cmp::min(offset, 8)));
  (fields, size)
}

fn new_type(kind : Symbol, size_of : u64, info : *const ()) -> Type {
  Type { size_of, kind, kind_info: info }
}

fn new_simple_type(kind : Symbol, size : u64) -> Type {
  new_type(kind, size, std::ptr::null())
}

pub fn tuple_type_internal(kind : Symbol, fields : &[Type]) -> Type {
  let (fields, size) = struct_offsets(fields);
  let info =
    Box::into_raw(Box::new(
      TupleInfo { fields: perm_slice(&fields)}))
    as *const ();
  new_type(kind, size, info)
}

pub fn tuple_type(c : &CoreTypes, fields : &[Type]) -> Type {
  tuple_type_internal(c.tuple_kind, fields)
}

pub fn array_type(c : &CoreTypes, bytes : u64) -> Type {
  new_simple_type(c.array_kind, bytes)
}

fn function_type_inner(c : &CoreTypes, args : &[Type], returns : Type, c_function : bool) -> Type {
  let info =
    Box::into_raw(Box::new(
      FunctionInfo { args: perm_slice(args), returns, c_function }))
    as *const ();
  new_type(c.function_kind, 8, info)
}

pub fn function_type(c : &CoreTypes, args : &[Type], returns : Type) -> Type {
  function_type_inner(c, args, returns, false)
}

pub fn c_function_type(c : &CoreTypes, args : &[Type], returns : Type) -> Type {
  function_type_inner(c, args, returns, true)
}

pub fn core_types(st : SymbolTable) -> CoreTypes {

  let primitive_kind = to_symbol(st, "primitive");
  let tuple_kind = to_symbol(st, "tuple");
  let function_kind = to_symbol(st, "function");
  let pointer_kind = to_symbol(st, "pointer");
  let array_kind = to_symbol(st, "array");

  let u64_tag = new_simple_type(primitive_kind, 8);
  let u32_tag = new_simple_type(primitive_kind, 4);
  let u16_tag = new_simple_type(primitive_kind, 2);
  let u8_tag = new_simple_type(primitive_kind, 1);
  let void_tag = new_simple_type(primitive_kind, 0);

  let type_tag = tuple_type_internal(tuple_kind, &[
    u64_tag,
    u64_tag,
    u64_tag,
  ]);

  let slice_tag = tuple_type_internal(tuple_kind, &[
    u64_tag,
    u64_tag,
  ]);

  CoreTypes {
    primitive_kind, tuple_kind, function_kind, pointer_kind, array_kind,
    type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_tag, slice_tag,
    core_types:
      vec![
        (to_symbol(st, "type"), type_tag),
        (to_symbol(st, "u64"), u64_tag),
        (to_symbol(st, "u32"), u32_tag),
        (to_symbol(st, "u16"), u16_tag),
        (to_symbol(st, "u8"), u8_tag),
        (to_symbol(st, "void"), void_tag),
        (to_symbol(st, "slice"), slice_tag),
      ],
  }
}

#[test]
fn test_types() {
  let st = symbols::symbol_table();
  let c = core_types(st);

  let a =
    tuple_type(&c, &[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]);
  let b =
    tuple_type(&c, &[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]);

  if a.size_of != 24 { panic!("tuple A alignment incorrect") }
  if b.size_of != 16 { panic!("tuple B alignment incorrect") }
}

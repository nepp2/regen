/// Defines the an extensible TypeInfo data structure to be used by the
/// compiler & interpreter.

use crate::symbols::Symbol;
use crate::perm_alloc::{Perm, PermSlice, perm, perm_slice, perm_slice_from_vec};
use std::fmt;

pub type TypeHandle = Perm<TypeInfo>;

#[derive(Copy, Clone)]
#[repr(u64)]
pub enum Primitive {
  U64 = 0,
  U32 = 1,
  U16 = 2,
  U8 = 3,
  Void = 4,
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(u64)]
pub enum Kind {
  Primitive = 1,
  Type = 2,
  Struct = 3,
  Function = 4,
  Pointer = 5,
  Array = 6,
  Macro = 7,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeInfo {
  pub size_of : u64,
  pub kind : Kind,
  pub info : u64,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TupleInfo {
  pub field_types : PermSlice<TypeHandle>,
  pub field_offsets : PermSlice<u64>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct StructInfo {
  pub field_names : PermSlice<Symbol>,
  pub field_types : PermSlice<TypeHandle>,
  pub field_offsets : PermSlice<u64>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct PointerInfo {
  pub points_to : TypeInfo,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct FunctionInfo {
  pub args : PermSlice<TypeHandle>,
  pub returns : TypeHandle,
  pub c_function : bool,
}

#[derive(Clone)]
pub struct CoreTypes {
  pub type_tag : TypeHandle,
  pub u64_tag : TypeHandle,
  pub u32_tag : TypeHandle,
  pub u16_tag : TypeHandle,
  pub u8_tag : TypeHandle,
  pub void_tag : TypeHandle,
  pub slice_tag : TypeHandle,
  pub string_tag : TypeHandle,

  pub array_types : Vec<TypeHandle>,

  pub core_types : Vec<(&'static str, TypeHandle)>,
}

pub fn type_as_primitive(t : &TypeInfo) -> Option<Primitive> {
  if let Kind::Primitive = t.kind {
    return Some(unsafe { std::mem::transmute(t.info) });
  }
  None
}

pub fn type_as_function(t : &TypeInfo) -> Option<&FunctionInfo> {
  if let Kind::Function = t.kind {
    return Some(unsafe { &*(t.info as *const FunctionInfo) })
  }
  None
}

pub fn type_as_struct(t : &TypeInfo) -> Option<&StructInfo> {
  if let Kind::Struct = t.kind {
    return Some(unsafe { &*(t.info as *const StructInfo) })
  }
  None
}

pub fn deref_pointer_type(t : &TypeInfo) -> Option<TypeHandle> {
  if let Kind::Pointer = t.kind {
    return Some(TypeHandle::from_u64(t.info))
  }
  None
}

pub fn type_as_macro(t : &TypeInfo) -> Option<TypeHandle> {
  if let Kind::Macro = t.kind {
    return Some(Perm::from_ptr(t.info as *mut TypeInfo))
  }
  None
}

pub fn round_up_multiple(v : u64, multiple_of : u64) -> u64 {
  ((v / multiple_of) + ((v % multiple_of > 0) as u64)) * multiple_of
}

fn round_up_power_of_2(v : u64) -> u64 {
  let mut power = 1;
  while power < v { power *= 2; }
  power
}

fn new_type(kind : Kind, size_of : u64, info : u64) -> TypeHandle {
  perm(TypeInfo { size_of, kind, info: info })
}

pub fn calculate_packed_field_offsets(field_types : &[TypeHandle]) -> (Vec<u64>, u64) {
  let mut fields = vec![];
  let mut offset = 0;
  for &t in field_types {
    let field_size = round_up_power_of_2(t.size_of);
    offset = round_up_multiple(offset, std::cmp::min(field_size, 8));
    fields.push(offset);
    offset += field_size;
  }
  let size = round_up_multiple(offset, round_up_power_of_2(std::cmp::min(offset, 8)));
  (fields, size)
}

pub fn tuple_type(field_types : PermSlice<TypeHandle>) -> TypeHandle {
  struct_type(perm_slice(&[]), field_types)
}

pub fn struct_type(field_names : PermSlice<Symbol>, field_types : PermSlice<TypeHandle>) -> TypeHandle {
  let (offsets, size) = calculate_packed_field_offsets(field_types.as_slice());
  let info =
    Box::into_raw(Box::new(
      StructInfo {
        field_names, field_types,
        field_offsets: perm_slice_from_vec(offsets),
      }
    ))
    as u64;
  new_type(Kind::Struct, size, info)
}

pub fn array_type(c : &CoreTypes, bytes : u64) -> TypeHandle {
  for t in &c.array_types {
    if t.size_of == bytes {
      return *t;
    }
  }
  new_type(Kind::Array, bytes, 0)
}

pub fn pointer_type(inner : TypeHandle) -> TypeHandle {
  new_type(Kind::Pointer, 8, Perm::to_u64(inner))
}

fn function_type_inner(args : &[TypeHandle], returns : TypeHandle, c_function : bool) -> TypeHandle {
  let info =
    Box::into_raw(Box::new(
      FunctionInfo { args: perm_slice(args), returns, c_function }))
    as u64;
  new_type(Kind::Function, 8, info)
}

pub fn function_type(args : &[TypeHandle], returns : TypeHandle) -> TypeHandle {
  function_type_inner(args, returns, false)
}

pub fn c_function_type(args : &[TypeHandle], returns : TypeHandle) -> TypeHandle {
  function_type_inner(args, returns, true)
}

pub fn core_types() -> CoreTypes {

  let u64_tag = new_type(Kind::Primitive, 8, Primitive::U64 as u64);
  let u32_tag = new_type(Kind::Primitive, 4, Primitive::U32 as u64);
  let u16_tag = new_type(Kind::Primitive, 2, Primitive::U16 as u64);
  let u8_tag = new_type(Kind::Primitive, 1, Primitive::U8 as u64);
  let void_tag = new_type(Kind::Primitive, 0, Primitive::Void as u64);

  let type_tag = new_type(Kind::Type, 8, 0);

  let slice_tag = tuple_type(perm_slice(&[
    pointer_type(u8_tag),
    u64_tag,
  ]));

  let string_tag = tuple_type(perm_slice(&[
    pointer_type(u8_tag),
    u64_tag,
  ]));

  let mut array_types = vec![];
  for i in 2..20 {
    array_types.push(new_type(Kind::Array, i * 8, 0));
  }

  CoreTypes {
    type_tag, u64_tag, u32_tag, u16_tag,
    u8_tag, void_tag, slice_tag, string_tag,

    array_types,

    core_types:
      vec![
        ("type", type_tag),
        ("u64", u64_tag),
        ("u32", u32_tag),
        ("u16", u16_tag),
        ("u8", u8_tag),
        ("void", void_tag),
        ("slice", slice_tag),
        ("string", string_tag),
      ],
  }
}

impl fmt::Display for TypeInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      Kind::Primitive => {
        let s = match type_as_primitive(self).unwrap() {
          Primitive::U64 => "u64",
          Primitive::U32 => "u32",
          Primitive::U16 => "u16",
          Primitive::U8 => "u8",
          Primitive::Void => "void",
        };
        write!(f, "{}", s)?;
      }
      Kind::Struct => {
        write!(f, "(struct ")?;
        let info = unsafe { &*(self.info as *const StructInfo) };
        for i in 0..info.field_names.len() {
          if let Some(name) = info.field_names.as_slice().get(i) {
            write!(f, "({} {}) ", name, info.field_types[i])?;
          }
          else {
            write!(f, "{} ", info.field_types[i])?;
          }
          
        }
        write!(f, ")")?;
      }
      Kind::Function => {
        let i = unsafe { &*(self.info as *const FunctionInfo) };
        write!(f, "(fn (")?;
        for &t in i.args.as_slice() {
          write!(f, "{} ", t)?;
        }
        write!(f, ") {})", i.returns)?;
      }
      Kind::Pointer => {
        let t = unsafe { &*(self.info as *const TypeInfo) };
        write!(f, "(ptr {})", t)?;
      }
      Kind::Array => {
        write!(f, "(array {})", self.size_of)?;
      }
      Kind::Macro => {
        write!(f, "macro")?;
      }
      Kind::Type => {
        write!(f, "type")?;
      }
    }
    Ok(())
  }
}

#[test]
fn test_types() {
  let c = core_types();
  let a =
    tuple_type(perm_slice(&[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]));
  let b =
    tuple_type(perm_slice(&[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]));

  if a.size_of != 24 { panic!("tuple A alignment incorrect") }
  if b.size_of != 16 { panic!("tuple B alignment incorrect") }
}

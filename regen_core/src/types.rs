/// Defines the an extensible TypeInfo data structure to be used by the
/// compiler & interpreter.

use crate::{symbols, perm_alloc};

use symbols::{Symbol, SymbolTable, to_symbol};
use perm_alloc::{Ptr, SlicePtr, perm, perm_slice, perm_slice_from_vec};
use std::fmt;

pub type TypeHandle = Ptr<TypeInfo>;

#[derive(Copy, Clone, PartialEq)]
#[repr(u64)]
pub enum Primitive {
  I64 = 0,
  I32 = 1,
  U64 = 2,
  U32 = 3,
  U16 = 4,
  U8 = 5,
  Void = 6,
  Bool = 7,
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
  Named = 7,
  Poly = 8,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeInfo {
  pub size_of : u64,
  pub kind : Kind,
  pub info : u64,
}

impl PartialEq for TypeInfo {
  fn eq(&self, rhs : &Self) -> bool {
    (self.kind == rhs.kind) && (self.size_of == rhs.size_of) && {
      match self.kind {
        Kind::Primitive => self.info == rhs.info,
        Kind::Named => type_as_named(self) == type_as_named(rhs),
        Kind::Poly => type_as_poly(self) == type_as_poly(rhs),
        Kind::Type => true,
        Kind::Array => type_as_array(self) == type_as_array(rhs),
        Kind::Function => type_as_function(self) == type_as_function(rhs),
        Kind::Pointer =>
          TypeHandle::from_u64(self.info) == TypeHandle::from_u64(rhs.info),
        Kind::Struct => type_as_struct(self) == type_as_struct(rhs),
      }
    }
  }
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct ArrayInfo {
  pub inner : TypeHandle,
  pub length : i64,
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct NamedInfo {
  pub inner : TypeHandle,
  pub name : Symbol,
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct PolyInfo {
  pub t : TypeHandle,
  pub param : TypeHandle,
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct StructInfo {
  pub field_names : SlicePtr<Symbol>,
  pub field_types : SlicePtr<TypeHandle>,
  pub field_offsets : SlicePtr<u64>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct PointerInfo {
  pub points_to : TypeHandle,
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub struct FunctionInfo {
  pub args : SlicePtr<TypeHandle>,
  pub returns : TypeHandle,
  pub c_function : bool,
}

#[derive(Clone)]
pub struct CoreTypes {
  pub type_tag : TypeHandle,
  pub i64_tag : TypeHandle,
  pub i32_tag : TypeHandle,
  pub u64_tag : TypeHandle,
  pub u32_tag : TypeHandle,
  pub u16_tag : TypeHandle,
  pub u8_tag : TypeHandle,
  pub void_tag : TypeHandle,
  pub bool_tag : TypeHandle,
  pub string_tag : TypeHandle,
  pub expr_tag : TypeHandle,
  pub expr_slice_tag : TypeHandle,
  pub symbol_tag : TypeHandle,
  pub signal_tag : TypeHandle,
  pub tick_event_tag : TypeHandle,

  pub core_types : Vec<(&'static str, TypeHandle)>,
}

pub fn is_number(t : TypeHandle) -> bool {
  use Primitive::*;
  if let Some(p) = type_as_primitive(&t) {
    match p {
      I64 | I32 | U64 | U32 | U16 | U8 => return true,
      _ => (),
    }
  }
  false
}

pub fn is_bool(t : TypeHandle) -> bool {
  type_as_primitive(&t) == Some(Primitive::Bool)
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

pub fn type_as_array(t : &TypeInfo) -> Option<&ArrayInfo> {
  if let Kind::Array = t.kind {
    return Some(unsafe { &*(t.info as *const ArrayInfo) })
  }
  None
}

pub fn deref_pointer_type(t : TypeHandle) -> Option<TypeHandle> {
  if let Kind::Pointer = t.kind {
    return Some(TypeHandle::from_u64(t.info))
  }
  None
}

pub fn type_as_named(t : &TypeInfo) -> Option<&NamedInfo> {
  if let Kind::Named = t.kind {
    return Some(unsafe { &*(t.info as *const NamedInfo) })
  }
  None
}

pub fn type_as_poly(t : &TypeInfo) -> Option<&PolyInfo> {
  if let Kind::Poly = t.kind {
    return Some(unsafe { &*(t.info as *const PolyInfo) })
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

pub fn struct_type(field_names : SlicePtr<Symbol>, field_types : SlicePtr<TypeHandle>) -> TypeHandle {
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

pub fn slice_type(c : &CoreTypes, st : SymbolTable, element_type : TypeHandle) -> TypeHandle {
  struct_type(
    perm_slice(&[to_symbol(st, "data"), to_symbol(st, "len")]),
    perm_slice(&[pointer_type(element_type), c.u64_tag]),
  )
}

pub fn array_type(inner : TypeHandle, length : i64) -> TypeHandle {
  let info = Box::into_raw(Box::new(ArrayInfo { inner, length })) as u64;
  new_type(Kind::Array, length as u64 * inner.size_of, info)
}

pub fn pointer_type(inner : TypeHandle) -> TypeHandle {
  new_type(Kind::Pointer, 8, Ptr::to_u64(inner))
}

pub fn named_type(name : Symbol, inner : TypeHandle) -> TypeHandle {
  let info = Box::into_raw(Box::new(NamedInfo { inner, name })) as u64;
  new_type(Kind::Named, inner.size_of, info)
}

pub fn poly_type(t : TypeHandle, param : TypeHandle) -> TypeHandle {
  let info = Box::into_raw(Box::new(PolyInfo { t, param })) as u64;
  new_type(Kind::Poly, t.size_of, info)
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

pub fn core_types(st : SymbolTable) -> CoreTypes {

  let i64_tag = new_type(Kind::Primitive, 8, Primitive::I64 as u64);
  let i32_tag = new_type(Kind::Primitive, 4, Primitive::I32 as u64);
  let u64_tag = new_type(Kind::Primitive, 8, Primitive::U64 as u64);
  let u32_tag = new_type(Kind::Primitive, 4, Primitive::U32 as u64);
  let u16_tag = new_type(Kind::Primitive, 2, Primitive::U16 as u64);
  let u8_tag = new_type(Kind::Primitive, 1, Primitive::U8 as u64);
  let void_tag = new_type(Kind::Primitive, 0, Primitive::Void as u64);
  let bool_tag = new_type(Kind::Primitive, 1, Primitive::Bool as u64);

  let data_sym = to_symbol(st, "data");
  let len_sym = to_symbol(st, "len");

  let expr_tag = named_type(to_symbol(st, "expr"), u64_tag);
  let expr_slice_tag = struct_type(
    perm_slice(&[data_sym, len_sym]),
    perm_slice(&[pointer_type(expr_tag), u64_tag]),
  );

  let type_tag = new_type(Kind::Type, 8, 0);

  let symbol_tag = named_type(to_symbol(st, "symbol"), u64_tag);
  let signal_tag = named_type(to_symbol(st, "signal"), u64_tag);

  let string_tag = struct_type(
    perm_slice(&[data_sym, len_sym]),
    perm_slice(&[pointer_type(u8_tag), u64_tag]),
  );

  let tick_event_tag = struct_type(
    perm_slice(&[
      to_symbol(st, "tick_interval"),
      to_symbol(st, "current_millisecond"),
    ]),
    perm_slice(&[i64_tag, i64_tag]),
  );

  let mut array_types = vec![];
  for i in 2..20 {
    array_types.push(new_type(Kind::Array, i * 8, 0));
  }

  CoreTypes {
    type_tag, i64_tag, i32_tag, u64_tag, u32_tag, u16_tag,
    u8_tag, void_tag, bool_tag, string_tag,
    expr_tag, expr_slice_tag, symbol_tag,
    signal_tag, tick_event_tag,

    core_types:
      vec![
        ("type", type_tag),
        ("u64", u64_tag),
        ("u32", u32_tag),
        ("u16", u16_tag),
        ("u8", u8_tag),
        ("i64", i64_tag),
        ("i32", i32_tag),
        ("i16", u16_tag), // TODO: fix
        ("i8", u8_tag), // TODO: fix
        ("void", void_tag),
        ("bool", bool_tag),
        ("expr", expr_tag),
        ("node_slice", expr_slice_tag),
        ("string", string_tag),
        ("symbol", symbol_tag),
        ("signal", signal_tag),
        ("tick_event", tick_event_tag),
      ],
  }
}

impl fmt::Display for TypeInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      Kind::Primitive => {
        let s = match type_as_primitive(self).unwrap() {
          Primitive::I64 => "i64",
          Primitive::I32 => "i32",
          Primitive::U64 => "u64",
          Primitive::U32 => "u32",
          Primitive::U16 => "u16",
          Primitive::U8 => "u8",
          Primitive::Void => "void",
          Primitive::Bool => "bool",
        };
        write!(f, "{}", s)?;
      }
      Kind::Struct => {
        write!(f, "(struct ")?;
        let info = unsafe { &*(self.info as *const StructInfo) };
        for i in 0..info.field_types.len() {
          write!(f, "({} {}) ", info.field_names[i], info.field_types[i])?;
        }
        write!(f, ")")?;
      }
      Kind::Function => {
        let i = unsafe { &*(self.info as *const FunctionInfo) };
        write!(f, "({} (", if i.c_function {"cfun"} else {"fn"})?;
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
        let t = unsafe { &*(self.info as *const ArrayInfo) };
        write!(f, "(sized_array {} {})", t.inner, t.length)?;
      }
      Kind::Named => {
        let t = unsafe { &*(self.info as *const NamedInfo) };
        write!(f, "{}", t.name)?;
      }
      Kind::Type => {
        write!(f, "type")?;
      }
      Kind::Poly => {
        let t = unsafe { &*(self.info as *const PolyInfo) };
        write!(f, "{}({})", t.t, t.param)?;
      }
    }
    Ok(())
  }
}

#[test]
fn test_types() {
  let c = core_types(symbols::symbol_table());
  let (_, a_size) = calculate_packed_field_offsets(
    &[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]
  );
  let (_, b_size) = calculate_packed_field_offsets(
    &[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]
  );
  if a_size != 24 { panic!("tuple A alignment incorrect") }
  if b_size != 16 { panic!("tuple B alignment incorrect") }
}

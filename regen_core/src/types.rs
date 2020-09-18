/// Defines the an extensible TypeInfo data structure to be used by the
/// compiler & interpreter.

use crate::perm_alloc::{Perm, PermSlice, perm, perm_slice, perm_slice_from_vec};
use crate::symbols;
use symbols::{Symbol, to_symbol, SymbolTable};
use std::fmt;

pub type TypeHandle = Perm<TypeInfo>;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TypeInfo {
  pub size_of : u64,
  pub kind : Symbol,
  pub kind_info : *const (),
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TupleInfo {
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
  pub primitive_kind : Symbol,
  pub tuple_kind : Symbol,
  pub function_kind : Symbol,
  pub pointer_kind : Symbol,
  pub array_kind : Symbol,
  pub macro_kind : Symbol,

  pub type_tag : TypeHandle,
  pub u64_tag : TypeHandle,
  pub u32_tag : TypeHandle,
  pub u16_tag : TypeHandle,
  pub u8_tag : TypeHandle,
  pub void_tag : TypeHandle,
  pub slice_tag : TypeHandle,

  pub array_types : Vec<TypeHandle>,

  pub core_types : Vec<(Symbol, TypeHandle)>,
}

impl CoreTypes {
  pub fn as_function(&self, t : &TypeInfo) -> Option<&FunctionInfo> {
    if t.kind != self.function_kind {
      return None;
    }
    Some(unsafe { &*(t.kind_info as *const FunctionInfo) })
  }

  pub fn as_macro(&self, t : &TypeInfo) -> Option<TypeHandle> {
    if t.kind != self.macro_kind {
      return None;
    }
    Some(Perm::from_ptr(t.kind_info as *mut TypeInfo))
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

fn new_type(kind : Symbol, size_of : u64, info : *const ()) -> TypeHandle {
  perm(TypeInfo { size_of, kind, kind_info: info })
}

fn new_simple_type(kind : Symbol, size : u64) -> TypeHandle {
  new_type(kind, size, std::ptr::null())
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

pub fn tuple_type_internal(kind : Symbol, fields : &[TypeHandle]) -> TypeHandle {
  let (offsets, size) = calculate_packed_field_offsets(fields);
  let info =
    Box::into_raw(Box::new(
      TupleInfo {
        field_types: perm_slice(&fields),
        field_offsets: perm_slice_from_vec(offsets),
      }
    ))
    as *const ();
  new_type(kind, size, info)
}

pub fn tuple_type(c : &CoreTypes, fields : &[TypeHandle]) -> TypeHandle {
  tuple_type_internal(c.tuple_kind, fields)
}

pub fn array_type(c : &CoreTypes, bytes : u64) -> TypeHandle {
  for t in &c.array_types {
    if t.size_of == bytes {
      return *t;
    }
  }
  new_simple_type(c.array_kind, bytes)
}

fn function_type_inner(c : &CoreTypes, args : &[TypeHandle], returns : TypeHandle, c_function : bool) -> TypeHandle {
  let info =
    Box::into_raw(Box::new(
      FunctionInfo { args: perm_slice(args), returns, c_function }))
    as *const ();
  new_type(c.function_kind, 8, info)
}

pub fn function_type(c : &CoreTypes, args : &[TypeHandle], returns : TypeHandle) -> TypeHandle {
  function_type_inner(c, args, returns, false)
}

pub fn c_function_type(c : &CoreTypes, args : &[TypeHandle], returns : TypeHandle) -> TypeHandle {
  function_type_inner(c, args, returns, true)
}

const PRIMITIVE_KIND : &'static str = "primitive";
const TUPLE_KIND : &'static str = "tuple";
const FUNCTION_KIND : &'static str = "function";
const POINTER_KIND : &'static str = "pointer";
const ARRAY_KIND : &'static str = "array";
const MACRO_KIND : &'static str = "macro";

pub fn core_types(st : SymbolTable) -> CoreTypes {

  let primitive_kind = to_symbol(st, PRIMITIVE_KIND);
  let tuple_kind = to_symbol(st, TUPLE_KIND);
  let function_kind = to_symbol(st, FUNCTION_KIND);
  let pointer_kind = to_symbol(st, POINTER_KIND);
  let array_kind = to_symbol(st, ARRAY_KIND);
  let macro_kind = to_symbol(st, MACRO_KIND);

  let u64_tag = new_simple_type(primitive_kind, 8);
  let u32_tag = new_simple_type(primitive_kind, 4);
  let u16_tag = new_simple_type(primitive_kind, 2);
  let u8_tag = new_simple_type(primitive_kind, 1);
  let void_tag = new_simple_type(primitive_kind, 0);

  let type_tag = new_simple_type(primitive_kind, 8);
  
  // tuple_type_internal(tuple_kind, &[
  //   u64_tag,
  //   u64_tag,
  //   u64_tag,
  // ]);

  let slice_tag = tuple_type_internal(tuple_kind, &[
    u64_tag,
    u64_tag,
  ]);

  let mut array_types = vec![];
  for i in 2..20 {
    array_types.push(new_simple_type(array_kind, i * 8));
  }

  CoreTypes {
    primitive_kind, tuple_kind, function_kind,
    pointer_kind, array_kind, macro_kind,

    type_tag, u64_tag, u32_tag, u16_tag, u8_tag, void_tag, slice_tag,

    array_types,

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

impl fmt::Display for TypeInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind.as_str() {
      PRIMITIVE_KIND => {
        write!(f, "prim({})", self.size_of)?;
      }
      TUPLE_KIND => {
        write!(f, "(")?;
        let i = unsafe { &*(self.kind_info as *const TupleInfo) };
        for &t in i.field_types.as_slice() {
          write!(f, "{}, ", t)?;
        }
        write!(f, ")")?;
      }
      FUNCTION_KIND => {
        let i = unsafe { &*(self.kind_info as *const FunctionInfo) };
        write!(f, "fn(")?;
        for &t in i.args.as_slice() {
          write!(f, "{}, ", t)?;
        }
        write!(f, ") : {}", i.returns)?;
      }
      POINTER_KIND => {
        let t = unsafe { &*(self.kind_info as *const TypeInfo) };
        write!(f, "ptr({})", t)?;
      }
      ARRAY_KIND => {
        write!(f, "byte_chunk({})", self.size_of)?;
      }
      MACRO_KIND => {
        write!(f, "macro")?;
      }
      _ => {
        write!(f, "{}", self.kind)?;
      }
    }
    Ok(())
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

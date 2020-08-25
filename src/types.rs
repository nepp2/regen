
use crate::symbols::{Symbol, to_symbol, symbol_table};
use crate::env::{Env, new_env};

#[derive(Copy, Clone)]
pub struct Type(*const TypeInfo);

impl Type {
  fn get(&self) -> &TypeInfo {
    unsafe { &*self.0 }
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

pub struct CoreTypes {
  type_tag : Type,
  u64_tag : Type,
  u32_tag : Type,
  u16_tag : Type,
  u8_tag : Type,
}

struct Field {
  t : Type,
  offset : u64,
}

#[derive(Copy, Clone)]
struct PackedArray {
  element_count : u64,
  element_size : u64,
  data : *const (),
}

fn to_packed_array<T>(types : Vec<T>) -> PackedArray {
  let element_count = types.len() as u64;
  let element_size = std::mem::size_of::<T>() as u64;
  let data = types.as_slice().as_ptr() as *const ();
  std::mem::forget(types);
  PackedArray { element_count, element_size, data }
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

fn new_type(env : &mut Env, name : &str, kind : &str, size : u64, info : *const ()) -> Type {
  let id = to_symbol(env.st, name);
  let kind = to_symbol(env.st, kind);
  let info = TypeInfo { size, id, kind, info };
  let t = Box::into_raw(Box::new(info));
  env.values.insert(id, t as u64);
  Type(t)
}

fn struct_type(env : &mut Env, name : &str, fields : &[Type]) -> Type {
  let (fields, size) = struct_offsets(fields);
  let info = Box::into_raw(Box::new(to_packed_array(fields))) as *const ();
  new_type(env, name, "struct", size, info)
}

fn primitive(env : &mut Env, name : &str, size : u64) -> Type {
  new_type(env, name, "primitive", size, std::ptr::null())
}

pub fn core_types(env : &mut Env) -> CoreTypes {

  let u64_tag = primitive(env, "u64", 8);
  let u32_tag = primitive(env, "u32", 4);
  let u16_tag = primitive(env, "u16", 2);
  let u8_tag = primitive(env, "u8", 1);
  let symbol_tag = primitive(env, "symbol", 8);
  let ptr_tag = primitive(env, "ptr", 8);

  let type_tag = struct_type(env, "type", &[
    u64_tag,
    symbol_tag,
    symbol_tag,
    ptr_tag
  ]);

  CoreTypes {
    type_tag, u64_tag, u32_tag, u16_tag, u8_tag,
  }
}

#[test]
fn test_types() {
  let st = symbol_table();
  let mut env = new_env(st);
  let c = core_types(&mut env);

  let a =
    struct_type(&mut env, "type",
      &[c.u16_tag, c.u32_tag, c.u16_tag, c.u64_tag]);
  let b =
    struct_type(&mut env, "type",
      &[c.u16_tag, c.u16_tag, c.u32_tag, c.u64_tag]);

  if a.get().size != 24 { panic!("struct A alignment incorrect") }
  if b.get().size != 16 { panic!("struct B alignment incorrect") }
}

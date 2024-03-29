use core::panic;
use std::hash::{Hash, Hasher};

use crate::{env, event_loop::ffi::*, parse::templates::template, parse::{self, Expr, ExprContent, ExprTag, Val}, regen_alloc::{self, alloc}, symbols::{Symbol, to_symbol}, types, types::{TypeHandle, c_function_type}};

use env::{Env, define_global};
use regen_alloc::{Ptr, SlicePtr, alloc_slice};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct RegenString {
  pub ptr : *const u8,
  pub len : u64,
}

impl PartialEq for RegenString {
  fn eq(&self, rhs : &Self) -> bool {
    self.as_str() == rhs.as_str()
  }
}
impl Eq for RegenString {}
impl Hash for RegenString {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_str().hash(state)
  }
}

impl RegenString {
  pub fn as_str(&self) -> &str {
    unsafe { 
      let s = std::slice::from_raw_parts(self.ptr, self.len as usize);
      std::str::from_utf8_unchecked(s)
    }
  }
}

pub fn from_string(s : String) -> RegenString {
  let len = s.len() as u64;
  let mut bytes = s.into_bytes();
  bytes.push(0);
  let ptr = bytes.as_ptr();
  std::mem::forget(bytes);
  RegenString { ptr, len }
}

pub extern "C" fn fail(s : Symbol) -> bool {
  panic!("failed with symbol {}", s)
}

extern {
  pub fn malloc(size: usize) -> *mut u8;
  pub fn free(ptr: *mut u8);
  pub fn memcpy(dest : *mut u8, src: *const u8, count : usize) -> *mut u8;
  pub fn memset(dest : *mut u8, val: i32, count : usize) -> *mut u8;
}

pub extern "C" fn symbol_display(sym : Symbol) {
  println!("symbol: {}", sym)
}

pub extern "C" fn ptr_type(t : TypeHandle) -> TypeHandle{
  types::pointer_type(t)
}

pub extern "C" fn fun_type(
  args_ptr : *const TypeHandle,
  num_args : i64,
  return_type : TypeHandle,
  c_fun : bool,
) -> TypeHandle{
  let args = unsafe { std::slice::from_raw_parts(args_ptr, num_args as usize) };
  if c_fun {
    types::c_function_type(args, return_type)
  }
  else {
    types::function_type(args, return_type)
  }
}

pub extern "C" fn sized_array_type(t : TypeHandle, length : i64) -> TypeHandle{
  types::array_type(t, length)
}

pub extern "C" fn named_type(name : Symbol, inner : TypeHandle) -> TypeHandle{
  types::named_type(name, inner)
}

pub extern "C" fn poly_type(t : TypeHandle, param : TypeHandle) -> TypeHandle{
  types::poly_type(t, param)
}

pub extern "C" fn struct_type(
  num_fields : u64,
  names_ptr : *const Symbol,
  types_ptr : *const TypeHandle,
) -> TypeHandle{
  let field_names = unsafe { std::slice::from_raw_parts(names_ptr, num_fields as usize) };
  let field_types = unsafe { std::slice::from_raw_parts(types_ptr, num_fields as usize) };
  types::struct_type(alloc_slice(field_names), alloc_slice(field_types))
}

pub extern "C" fn expr_display(expr : Expr) {
  println!("{}", expr);
}

pub extern "C" fn calculate_packed_field_offsets(
  field_types : &SlicePtr<TypeHandle>,
  field_offsets : &mut SlicePtr<u64>,
) -> u64
{
  let (offsets, size_of) = types::calculate_packed_field_offsets(field_types.as_slice());
  *field_offsets = alloc_slice(offsets);
  size_of
}

pub extern "C" fn template_quote(e : Expr, args_ptr : *const Expr, num_args : u64) -> Expr {
  let args = unsafe { std::slice::from_raw_parts(args_ptr, num_args as usize) };
  template(e, args)
}

pub extern "C" fn new_symbol_expr(env : Env, s : Ptr<RegenString>, e : Expr) -> Expr {
  let sym = to_symbol(env.st, s.as_str());
  parse::expr(ExprTag::Name, ExprContent::Sym(sym), e.loc())
}

pub extern "C" fn new_string_expr(_env : Env, s : Ptr<RegenString>, e : Expr) -> Expr {
  let s = alloc(*s);
  parse::expr(ExprTag::LiteralVal, ExprContent::Value(Val::String(s)), e.loc())
}

pub extern "C" fn type_sizeof(t : TypeHandle) -> u64 {
  t.size_of
}

pub extern "C" fn type_display(t : TypeHandle) {
  println!("{}", t);
}

pub fn load_ffi_libs(e : Env) {
  let bool_type = e.c.bool_tag;
  let u64 = e.c.u64_tag;
  let i64 = e.c.i64_tag;
  let u32 = e.c.u32_tag;
  let signal_tag = e.c.signal_tag;
  let reactive_constructor_tag = e.c.reactive_constructor_tag;
  let expr = e.c.expr_tag;
  let type_tag = e.c.type_tag;
  let type_tag_ptr = types::pointer_type(type_tag);
  let symbol = e.c.symbol_tag;
  let symbol_ptr = types::pointer_type(symbol);
  let string_ptr = types::pointer_type(e.c.string_tag);

  let c = &e.c;
  for (n, t) in &c.core_types {
    define_global(e, n, Ptr::to_u64(t.0), e.c.type_tag);
  }

  let void = e.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let env_tag = void_ptr;

  define_global(e, "env", Ptr::to_u64(e), env_tag);

  define_global(e, "event_loop", Ptr::to_u64(e.event_loop), void_ptr);


  // ----------- Bind type constructor functions

  define_global(e, "ptr_type", ptr_type as u64,
    c_function_type(&[type_tag], type_tag));

  define_global(e, "fun_type", fun_type as u64,
    c_function_type(&[type_tag_ptr, i64, type_tag, bool_type], type_tag));
  
  define_global(e, "sized_array_type", sized_array_type as u64,
    c_function_type(&[type_tag, i64], type_tag));

  define_global(e, "struct_type", struct_type as u64,
    c_function_type(&[i64, symbol_ptr, type_tag_ptr], type_tag));

  define_global(e, "named_type", named_type as u64,
    c_function_type(&[symbol, type_tag], type_tag));

  define_global(e, "poly_type", poly_type as u64,
    c_function_type(&[type_tag, type_tag], type_tag));

  // ----------- Bind core system functions ------------

  define_global(e, "fail", fail as u64,
    c_function_type(&[u64], u64));

  define_global(e, "malloc", malloc as u64,
    c_function_type(&[u64], void_ptr));

  define_global(e, "free", free as u64,
    c_function_type(&[void_ptr], void));

  define_global(e, "memcpy", memcpy as u64,
    c_function_type(&[void_ptr, void_ptr, u64], void_ptr));

  define_global(e, "memset", memset as u64,
    c_function_type(&[void_ptr, u32, u64], void_ptr));
  
  // ----------- Bind signal functions ------------

  define_global(e, "new_watcher_constructor", new_watcher_constructor as u64,
    c_function_type(
      &[env_tag, string_ptr],
      reactive_constructor_tag));

  define_global(e, "new_timer_constructor", new_timer_constructor as u64,
    c_function_type(
      &[env_tag, i64],
      reactive_constructor_tag));

  define_global(e, "new_state_constructor", new_state_constructor as u64,
    c_function_type(
      &[signal_tag, type_tag, void_ptr, void_ptr],
      reactive_constructor_tag));

  define_global(e, "new_poll_constructor", new_poll_constructor as u64,
    c_function_type(
      &[signal_tag, type_tag, void_ptr],
      reactive_constructor_tag));

  // ----------- Bind metaprogramming functions ------------
  
  define_global(e, "template_quote", template_quote as u64,
    c_function_type(&[expr, types::pointer_type(expr), i64], expr));

  define_global(e, "new_symbol_expr", new_symbol_expr as u64,
    c_function_type(&[env_tag, string_ptr, expr], expr));

  define_global(e, "new_string_expr", new_string_expr as u64,
    c_function_type(&[env_tag, string_ptr, expr], expr));

  // ----------- Bind language introspection functions ------------

  define_global(e, "symbol_display", symbol_display as u64,
    c_function_type(&[u64], void));

  define_global(e, "calculate_packed_field_offsets", calculate_packed_field_offsets as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "type_sizeof", type_sizeof as u64,
    c_function_type(&[type_tag], u64));

  define_global(e, "type_display", type_display as u64,
    c_function_type(&[u64], void));
}


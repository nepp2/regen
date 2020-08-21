use crate::symbols::{Symbol, SymbolTable, to_symbol};
use std::collections::HashMap;
use std::path::Path;
use std::ffi::CString;
use libloading::{Library, Symbol as LibSymbol};

/// Environment for interpreter
pub struct Env {
  pub values : HashMap<Symbol, u64>,
  pub st : SymbolTable,
}

pub extern "C" fn c_add(a : u64, b : u64) -> u64 {
  a + b
}

#[repr(C)]
pub struct TestRect {
  x : u32,
  y : u32,
  w : u32,
  h : u32,
}

pub extern "C" fn test_struct(r : TestRect) -> bool {
  r.x == 1 && r.y == 2 && r.w == 3 && r.h == 4
}

extern {
  pub fn malloc(size: usize) -> *mut u8;
  pub fn free(ptr: *mut u8);
  pub fn memcpy(dest : *mut u8, src: *const u8, count : usize) -> *mut u8;
}

pub extern "C" fn env_insert(env : &mut Env, sym : Symbol, value : u64) {
  env.values.insert(sym, value);
}

pub extern "C" fn env_get(env : &Env, sym : Symbol) -> u64 {
  env.values[&sym]
}

pub extern "C" fn print_symbol(sym : Symbol) {
  println!("symbol: {}", sym)
}

pub extern "C" fn load_library(path : Symbol) -> *const Library {
  load_library_str(path.as_str())
}

pub extern "C" fn load_library_str(path : &str) -> *const Library {
  let path = Path::new(path);
  let r = Library::new(path);
  if r.is_err() {
    println!("Failed to load library {}", path.display());
    return std::ptr::null();
  }
  let lib = r.unwrap();
  Box::into_raw(Box::new(lib))
}

pub extern "C" fn load_library_symbol(lib : &Library, symbol : Symbol) -> *const () {
  let s = CString::new(symbol.as_str()).unwrap();
  unsafe {
    let symbol: Option<LibSymbol<*const ()>> =
      lib.get(s.as_bytes_with_nul()).ok();
    symbol.map(|sym| sym.into_raw().into_raw() as *const ()).unwrap_or(std::ptr::null())
  }
}

pub fn new_env(st : SymbolTable) -> Box<Env> {
  let env_ptr = Box::into_raw(Box::new(Env {
    values: Default::default(),
    st
  }));
  let mut env = unsafe { Box::from_raw(env_ptr) };
  env.values.insert(to_symbol(st, "c_add"), c_add as u64);
  env.values.insert(to_symbol(st, "test_struct"), test_struct as u64);
  env.values.insert(to_symbol(st, "malloc"), malloc as u64);
  env.values.insert(to_symbol(st, "free"), free as u64);
  env.values.insert(to_symbol(st, "memcpy"), memcpy as u64);
  env.values.insert(to_symbol(st, "env"), env_ptr as u64);
  env.values.insert(to_symbol(st, "env_insert"), env_insert as u64);
  env.values.insert(to_symbol(st, "env_get"), env_get as u64);
  env.values.insert(to_symbol(st, "print_symbol"), print_symbol as u64);
  env.values.insert(to_symbol(st, "load_library"), load_library as u64);
  env.values.insert(to_symbol(st, "load_library_symbol"), load_library_symbol as u64);
  env
}


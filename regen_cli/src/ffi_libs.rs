
use regen_core::{env::{Env, define_global}, ffi_libs::RegenString, hotload, parse::{self, Expr}, symbols::{Symbol, to_symbol}, types, types::c_function_type};
use std::fs;
use std::path::Path;
use std::ffi::CString;
use libloading::{Library, Symbol as LibSymbol};

#[cfg(not(test))]
const LIB_PATH : &'static str = ".";
#[cfg(test)]
const LIB_PATH : &'static str = "..";

fn load_code(file_name : &str) -> String {
  let path = format!("{}/{}", LIB_PATH, file_name);
  match fs::read_to_string(&path) {
    Ok(s) => s,
    Err(e) => panic!("{}", e),
  }
}

pub extern "C" fn load_expr(env : Env, file_name : &RegenString) -> Expr {
  let file_name = file_name.as_str();
  let code = load_code(file_name);
  let module_name = to_symbol(env.st, file_name);
  parse::parse_module(env.st, module_name, &code).unwrap()
}

pub extern "C" fn load_library(path : Symbol) -> *const Library {
  load_library_str(path.as_str())
}

fn load_library_str(path : &str) -> *const Library {
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

fn preload_file(env : Env, path : &str) {
  let code = load_code(path);
  hotload::preload_module(env, path, &code)
}

pub fn bind_libs(env : Env) {
  let symbol_tag = env.c.symbol_tag;
  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let string_ptr = types::pointer_type(env.c.string_tag);
  let expr_tag = env.c.expr_tag;
  define_global(env, "load_expr", load_expr as u64,
    c_function_type(&[void_ptr, string_ptr], expr_tag));
  define_global(env, "load_library", load_library as u64,
    c_function_type(&[symbol_tag], void_ptr));
  define_global(env, "load_library_symbol", load_library_symbol as u64,
    c_function_type(&[void_ptr, symbol_tag], void_ptr));

  preload_file(env, "examples/lib/prelude.gen");
}

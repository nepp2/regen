
use regen_core::{env, env::{CellUid, Env, define_global}, ffi_libs::RegenString, hotload, symbols::{Symbol, to_symbol}, types, types::c_function_type};
use std::fs;
use std::path::Path;
use std::ffi::CString;
use libloading::{Library, Symbol as LibSymbol};

#[cfg(not(test))]
const LIB_PATH : &'static str = ".";
#[cfg(test)]
const LIB_PATH : &'static str = "..";

pub extern "C" fn include(env : Env, file_name : &RegenString) {
  let file_name = file_name.as_str();
  let module_namespace = to_symbol(env.st, "__module");
  let path_symbol = to_symbol(env.st, file_name);
  let uid = CellUid::def(path_symbol, env::new_namespace(&[module_namespace]));
  if env::get_cell_value(env, uid).is_none() {
    let path = format!("{}/{}", LIB_PATH, file_name);
    let code =
      fs::read_to_string(&path)
      .expect("Something went wrong reading the file");
    hotload::interpret_module(file_name, &code, env);
    let u64 = env.c.u64_tag;
    define_global(env, path_symbol.as_str(), path_symbol.as_u64(), u64);
  }
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

pub fn bind_libs(env : Env) {
  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let string_ptr = types::pointer_type(env.c.string_tag);
  define_global(env, "include", include as u64,
    c_function_type(&[void_ptr, string_ptr], void));
  define_global(env, "load_library", load_library as u64,
    c_function_type(&[u64], u64));
  define_global(env, "load_library_symbol", load_library_symbol as u64,
    c_function_type(&[u64, u64], u64));
}

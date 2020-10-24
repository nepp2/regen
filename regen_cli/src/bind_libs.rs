
use regen_core::{
  env::Env,
  symbols::{Symbol, to_symbol},
  types::c_function_type,
  interpret,
};
use std::fs;
use std::path::Path;
use std::ffi::CString;
use libloading::{Library, Symbol as LibSymbol};

#[cfg(not(test))]
const LIB_PATH : &'static str = "examples/lib";
#[cfg(test)]
const LIB_PATH : &'static str = "../examples/lib";

pub extern "C" fn include(mut env : Env, file_name : Symbol) {
  let path_symbol = to_symbol(env.st, format!("module::{}", file_name));
  if env.get(path_symbol).is_none() {
    let path = format!("{}/{}.gen", LIB_PATH, file_name.as_str());
    let code =
      fs::read_to_string(&path)
      .expect("Something went wrong reading the file");
    interpret::interpret_file(&code, env);
    let u64 = env.c.u64_tag;
    env.insert(path_symbol, path_symbol.as_u64(), u64);
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

pub fn bind_libs(mut env : Env) {
  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;
  env.insert_str("include", include as u64,
    c_function_type(&[u64], void));
  env.insert_str("load_library", load_library as u64,
    c_function_type(&[u64], u64));
  env.insert_str("load_library_symbol", load_library_symbol as u64,
    c_function_type(&[u64, u64], u64));
}

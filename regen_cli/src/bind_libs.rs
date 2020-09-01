
use regen_core::{
  env::Env,
  symbols::Symbol,
};
use std::path::Path;
use std::ffi::CString;
use libloading::{Library, Symbol as LibSymbol};

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

pub fn bind_libs(env : &mut Env) {
  env.insert_str("load_library", load_library as u64, env.c.u64_tag);
  env.insert_str("load_library_symbol", load_library_symbol as u64, env.c.u64_tag);
}

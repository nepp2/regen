
// A lot of crazy unsafe stuff happens here

use std::collections::HashMap;
use std::fmt;

#[derive(Copy, Clone, PartialEq)]
struct StrPtr {
  ptr : *const u8,
  len : usize,
}

impl StrPtr {
  fn as_str(&self) -> &'static str {
    unsafe {
      let slice = std::slice::from_raw_parts(self.ptr, self.len);
      std::str::from_utf8_unchecked(slice)
    }
  }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Symbol(usize);

#[derive(Default)]
struct Symbols {
  /// At the moment nothing should be removed from this, because the length is used to generate
  /// unique symbols and there is an unsafe pointer operation in the Borrow implementation which
  /// relies on the string data never being moved/deallocated.
  strings : Vec<&'static str>,

  /// symbols carry an index into the strings vec
  symbol_set : HashMap<&'static str, Symbol>,
}

impl fmt::Debug for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s : &str = get_symbols().strings[self.0];
    write!(f, "Symbol({})", s)
  }
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s : &str = get_symbols().strings[self.0];
    write!(f, "{}", s)
  }
}

fn make_static(s: String) -> &'static str {
  let v = unsafe {
    std::mem::transmute::<&str, & 'static str>(s.as_ref())
  };
  std::mem::forget(s);
  v
}

pub fn to_symbol(s : &str) -> Symbol {
  let symbols = get_symbols();
  let proxy = symbols.symbol_set.get(s);
  if proxy.is_some() { return Symbol(proxy.unwrap().0) };
  let s = make_static(s.to_string());
  let sym = Symbol(symbols.strings.len());
  symbols.strings.push(s);
  symbols.symbol_set.insert(s, sym);
  sym
}

fn get_symbols() -> &'static mut Symbols {
  unsafe {
    if !SYMBOLS_INITIALISED {
      SYMBOLS = Box::into_raw(Box::new(Default::default()));
      SYMBOLS_INITIALISED = true;
    }
    &mut *SYMBOLS
  }
}

static mut SYMBOLS_INITIALISED: bool = false;
static mut SYMBOLS: *mut Symbols = std::ptr::null_mut();

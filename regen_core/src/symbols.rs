/// A symbol table. Hashes & caches strings so that they can
/// be passed around as a pointer, which can then be compared
/// without dereferencing.

use std::collections::HashMap;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Symbol(*const *const str);

impl Symbol {
  pub fn as_u64(self) -> u64 {
    self.0 as u64
  }

  pub fn as_str(self) -> &'static str {
    unsafe { &**self.0 }
  }
}

#[derive(Default)]
struct Symbols {
  /// symbols carry an index into the strings vec
  symbol_set : HashMap<&'static str, Symbol>,
}

#[derive(Copy, Clone)]
pub struct SymbolTable(*mut Symbols);

impl fmt::Debug for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO
    write!(f, "Symbol({})", unsafe { &**self.0 } )
  }
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO
    write!(f, "{}", unsafe { &**self.0} )
  }
}

fn make_static(s: String) -> &'static str {
  let v = unsafe {
    std::mem::transmute::<&str, & 'static str>(s.as_ref())
  };
  std::mem::forget(s);
  v
}

pub fn symbol_table() -> SymbolTable {
  SymbolTable(Box::into_raw(Box::new(Default::default())))
}

pub fn to_symbol(symbols : SymbolTable, s : &str) -> Symbol {
  let symbols = unsafe { &mut *symbols.0 };
  let proxy = symbols.symbol_set.get(s);
  if proxy.is_some() { return Symbol(proxy.unwrap().0) };
  let s = make_static(s.to_string());
  // TODO: replace use of Box with a bump allocator/slotmap/something
  // OR: store the string data in an unsized struct with the size included,
  // to reduce indirection.
  let sym = Symbol(Box::into_raw(Box::new(s as *const str)));
  symbols.symbol_set.insert(s, sym);
  sym
}

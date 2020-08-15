use crate::symbols::{Symbol, SymbolTable};
use std::collections::HashMap;

/// Environment for interpreter
pub struct Env {
  pub values : HashMap<Symbol, u64>,
  pub st : SymbolTable,
}

impl Env {
  pub fn new(st : SymbolTable) -> Self {
    Env {
      values: HashMap::new(), st
    }
  }
}

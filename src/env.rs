use crate::symbols::Symbol;
use std::collections::HashMap;

/// Environment for interpreter
pub type Env = HashMap<Symbol, u64>;
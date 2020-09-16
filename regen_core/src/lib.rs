#![allow(dead_code)]

mod perm_alloc;
pub mod symbols;
mod parse;
pub mod env;
mod bytecode;
mod compile;
pub mod interpret;
mod ffi;
pub mod types;

pub fn new_env() -> env::Env {
    env::new_env(symbols::symbol_table())
}

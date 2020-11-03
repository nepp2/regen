#![allow(dead_code)]

mod perm_alloc;
pub mod symbols;
mod parse;
pub mod env;
mod bytecode;
mod compile;
pub mod interpret;
mod ffi;
mod interop;
pub mod types;
pub mod debug;

pub fn new_env() -> env::Env {
    env::new_env(symbols::symbol_table())
}

#![allow(dead_code)]

mod perm_alloc;
pub mod symbols;
mod parse;
pub mod env;
mod bytecode;
mod compile;
mod interpret;
mod ffi;
pub mod types;

#[cfg(test)]
mod test;

pub use interpret::interpret;

pub fn new_env() -> Box<env::Env> {
    env::new_env(symbols::symbol_table())
}

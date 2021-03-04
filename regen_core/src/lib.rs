pub mod perm_alloc;
pub mod symbols;
mod parse;
pub mod env;
mod bytecode;
mod compile;
pub mod interpret;
mod ffi_ccall;
pub mod ffi_libs;
pub mod types;
pub mod debug;
pub mod event_loop;
pub mod hotload;
mod error;

// TODO: is this needed?
// mod graph;

pub fn new_env() -> env::Env {
  env::new_env(symbols::symbol_table())
}

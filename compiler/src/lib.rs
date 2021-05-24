pub mod perm_alloc;
pub mod symbols;
pub mod parse;
pub mod env;
mod bytecode;
mod dependencies;
mod codegen;
pub mod interpret;
mod ffi_ccall;
pub mod ffi_libs;
pub mod types;
pub mod debug;
pub mod event_loop;
pub mod hotload;
mod error;

pub fn new_env() -> env::Env {
  env::new_env(symbols::symbol_table())
}

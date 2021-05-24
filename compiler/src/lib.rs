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
  let st = symbols::symbol_table();
  let c = perm_alloc::perm(types::core_types(st));
  env::new_env(st, c)
}

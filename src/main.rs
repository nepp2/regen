
#![allow(dead_code)]

mod perm_alloc;
mod symbols;
mod parse;
mod env;
mod bytecode;
mod compile;
mod interpret;
mod ffi;

#[cfg(test)]
mod test;

// #[cfg(test)]
// #[macro_use] extern crate rusty_fork;

use interpret::run_file;

fn main() {
  run_file("examples/scratchpad.gen", bytecode::read_bytecode);
}

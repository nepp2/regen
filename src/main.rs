
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

fn main() {
  //interpret::run_file("examples/scratchpad.gen");
  interpret::run_file("examples/sdl_example.gen");
}

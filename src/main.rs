
#![allow(dead_code)]

mod symbols;
mod parse;
mod env;
mod bytecode;
mod interpret;
mod ffi;

use std::fs;

fn main() {
  let contents =
    fs::read_to_string("examples/core.gen")
    .expect("Something went wrong reading the file");
  
  // println!("file contents:\n\n{}", contents);

  let ast = parse::parse(contents);
  interpret::interpret(&ast);
}


#![allow(dead_code)]

mod symbols;
mod parse;
mod env;
mod bytecode;
mod interpret;
mod ffi;

use std::fs;

fn run_file(path : &str) {
  let contents =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let ast = parse::parse(contents);
  interpret::interpret(&ast);
}

#[test]
fn run_test_cases() {
  run_file("examples/test.gen");
}

fn main() {
  run_file("examples/core.gen");
}

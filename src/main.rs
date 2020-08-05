
#![allow(dead_code)]

mod perm_alloc;
mod symbols;
mod parse;
mod env;
mod bytecode;
mod interpret;
mod ffi;

use std::fs;
use std::path::Path;

fn run_file(path : impl AsRef<Path>) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let ast = parse::parse(&code);
  interpret::interpret(&ast, &code);
}

#[test]
fn run_test_cases() {
  let tests = fs::read_dir("examples/tests")
    .expect("could not open test directory")
    .flatten()
    .filter(|x|
      x.path().extension() == Some(std::ffi::OsStr::new("gen"))
    );
  for f in tests {
    run_file(f.path());
  }
}

fn main() {
  run_file("examples/scratchpad.gen");
}

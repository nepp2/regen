
use crate::interpret::interpret;
use crate::env::new_env;
use crate::symbols;

use std::fs;
use std::path::Path;

fn run_file(path : impl AsRef<Path>) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let st = symbols::symbol_table();
  let mut env = new_env(st);
  interpret(&code, &mut env);
}

#[test]
fn run_test_cases() {
  let tests = fs::read_dir("../examples/tests")
    .expect("could not open test directory")
    .flatten()
    .filter(|x|
      x.path().extension() == Some(std::ffi::OsStr::new("gen"))
    );
  for f in tests {
    run_file(f.path());
  }
}

#[test]
fn test_milestone_1() {
  run_file("../examples/milestones/1_concise.gen");
}
  
#[test]
fn test_milestone_2() {
  run_file("../examples/milestones/2_expose_env.gen");
}

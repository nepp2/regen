
use crate::interpret::run_file;

use std::fs;

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

#[test]
fn test_milestone_1() {
  run_file("examples/milestones/1_concise.gen");
}
  
#[test]
fn test_milestone_2() {
  run_file("examples/milestones/2_expose_env.gen");
}


use crate::{ compile, bytecode, interpret };
use interpret::run_file;

use std::fs;

// Runs the tests in isolated processes, because they do unsafe things and could pollute each other.
// rusty_fork_test! {

  #[test]
  fn run_test_cases() {
    let tests = fs::read_dir("examples/tests")
      .expect("could not open test directory")
      .flatten()
      .filter(|x|
        x.path().extension() == Some(std::ffi::OsStr::new("gen"))
      );
    for f in tests {
      run_file(f.path(), bytecode::read_bytecode);
    }
  }
  
  #[test]
  fn test_milestone_1() {
    run_file("examples/milestone1_concise.gen", compile::compile_function);
  }
    
//}


use crate::interpret::interpret_file;
use crate::env::new_env;
use crate::symbols;

use std::fs;
use std::path::Path;

//use rusty_fork::rusty_fork_test;

fn run_file(path : impl AsRef<Path>) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let st = symbols::symbol_table();
  let env = new_env(st);
  interpret_file(&code, env);
}

//rusty_fork_test! {

  #[test]
  fn test_c_function() {
    run_file("../examples/tests/c_function.gen");
  }

  #[test]
  fn test_call_function() {
    run_file("../examples/tests/call_function.gen");
  }

  #[test]
  fn test_milestone_1() {
    run_file("../examples/milestones/1_concise.gen");
  }
    
  #[test]
  fn test_milestone_2() {
    run_file("../examples/milestones/2_expose_env.gen");
  }

  #[test]
  fn test_milestone_3() {
    run_file("../examples/milestones/3_struct_ish.gen");
  }

  #[test]
  fn test_milestone_4() {
    run_file("../examples/milestones/4_struct_return.gen");
  }

  #[test]
  fn test_milestone_5() {
    run_file("../examples/milestones/5_macros.gen");
  }

//}
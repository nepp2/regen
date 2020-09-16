
use crate::run_file;

//use rusty_fork::rusty_fork_test;

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
    run_file("../examples/milestones/3_tuples.gen");
  }

  #[test]
  fn test_milestone_4() {
    run_file("../examples/milestones/4_macros.gen");
  }

//}

use crate::run_file;

//use rusty_fork::rusty_fork_test;

//rusty_fork_test! {

  #[test]
  fn test_c_parity() {
    run_file("../examples/milestones/c_parity.gen");
  }

//}
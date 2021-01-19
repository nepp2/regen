#![allow(dead_code)]

mod watcher;
mod ffi_libs;
mod hotload_watcher;

#[cfg(test)]
mod test;

use regen_core::{interpret::interpret_file, new_env};
use std::fs;

pub fn run_file(path : &str) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let env = new_env();
  ffi_libs::bind_libs(env);
  interpret_file(path, &code, env);
}

fn main(){
  let args: Vec<String> = std::env::args().collect();
  let args: Vec<&str> = args.iter().map(|s| s.as_ref()).collect();
  match &args[1..] {
    ["hotload", path] => {
      hotload_watcher::watch_file(path)
    }
    ["watch", path] => {
      watcher::watch(path.as_ref())
    }
    ["watch"] => watcher::watch("examples/scratchpad.gen"),
    ["run", path] => {
      run_file(path);
    }
    [] => {
      run_file("examples/scratchpad.gen");
    },
    args => {
      println!("unrecognised arguments {:?}", args);
    }
  }
}

#![allow(dead_code)]

mod watcher;
mod bind_libs;

#[cfg(test)]
mod test;

use regen_core::{interpret::interpret_file, new_env};
use std::fs;
use std::path::Path;

pub fn run_file(path : impl AsRef<Path>) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let env = new_env();
  bind_libs::bind_libs(env);
  interpret_file(&code, env);
}

fn main(){
  let args: Vec<String> = std::env::args().collect();
  let args: Vec<&str> = args.iter().map(|s| s.as_ref()).collect();
  match &args[1..] {
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

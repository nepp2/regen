#![allow(dead_code)]

mod perm_alloc;
mod symbols;
mod parse;
mod env;
mod bytecode;
mod compile;
mod interpret;
mod ffi;
mod types;
mod watcher;

#[cfg(test)]
mod test;

// #[cfg(test)]
// #[macro_use] extern crate rusty_fork;

fn main(){
  let args: Vec<String> = std::env::args().collect();
  let args: Vec<&str> = args.iter().map(|s| s.as_ref()).collect();
  match &args[1..] {
    ["watch", path] => {
      watcher::watch(path.as_ref())
    }
    ["watch"] => watcher::watch("code/scratchpad.code"),
    ["run", path] => {
      interpret::run_file(path);
    }
    [] => {
      watcher::watch("examples/scratchpad.gen");
    },
    args => {
      println!("unrecognised arguments {:?}", args);
    }
  }
}

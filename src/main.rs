
#![allow(dead_code)]

mod symbols;
mod parse;
mod bytecode;
mod ffi;
mod interpret;

use std::fs;

fn main() {
  let contents =
    fs::read_to_string("examples/core.gen")
    .expect("Something went wrong reading the file");
  println!("file contents:\n\n{}", contents);
  let ast = parse::parse(contents);
  let bc = bytecode::codegen(&ast);

  println!();
  for b in bc.blocks.iter() {
    println!("Block {:?}", b.id);
    let end = b.start_op + b.num_ops;
    for (i, op) in bc.ops[b.start_op..end].iter().enumerate() {
      let i = b.start_op + i;
      print!("   {}: ", i);
      if let Some(varname) = op.varname {
        print!("let {} = ", varname);
      }
      println!("{:?}", op.content);
    }
  }
}

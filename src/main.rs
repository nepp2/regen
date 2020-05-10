
#![allow(dead_code)]

mod parse;
mod ffi;

use crate::parse::{parse, children};

use std::fs;

#[derive(Copy, Clone)]
enum TypeTag {
  Byte,
  Tuple,
  Array,
}

/// Stack frame byte offset to a specific value
#[derive(Copy, Clone)]
struct ValueOffset {
  offset : u32,
}

#[derive(Copy, Clone)]
enum Op {
  LiteralU8(ValueOffset, u8),
  Set{ src: ValueOffset, dest: ValueOffset, size: u32},
  CJump{ cond: ValueOffset, then_block: usize, else_block: usize },
  Add{ a: ValueOffset, b: ValueOffset, dest: ValueOffset },
  Error,
}

#[derive(Copy, Clone)]
struct Symbol(u64);

struct Block {
  id : Symbol,
  start : u64,
  len : u64,
}

struct Function {
  name : Symbol,
  ops : Vec<Op>,
  blocks : Vec<Block>,
}

union TypeUnion {
  tuple: Tuple,
  name: Name,
  array: Array,
}

#[derive(Copy, Clone)]
struct Type {
  tag : TypeTag,
  content : *mut TypeUnion,
}

#[derive(Copy, Clone)]
struct Tuple {
  member_count : u64,
  first_member : *mut Type,
}

#[derive(Copy, Clone)]
struct Name {
  symbol : u64,
  type_value : Type,
}

#[derive(Copy, Clone)]
struct Array {
  length : u64,
  type_value : Type,
}

struct Def {
  symbol : u64,
  type_val : Type,
}

struct Environment {
  symbol_table : Vec<String>,
  defs : Vec<Def>,
}

unsafe fn interpret(f : &Function) {
  let mut i = 0;
  let mut bytes : Vec<u8> = Vec::with_capacity(2048);
  loop {
    match &f.ops[i] {
      Op::LiteralU8(dest, value) => {
        bytes[dest.offset as usize] = *value;
      }
      Op::Set{ src, dest, size} => {
        let src = (src.offset as usize)..((src.offset + size) as usize);
        bytes.as_mut_slice().copy_within(src, dest.offset as usize)
      }
      Op::CJump{ cond, then_block, else_block } => {
        i = if bytes[cond.offset as usize] == 0
          { *else_block } else { *then_block };
        continue;
      }
      Op::Add{a, b, dest} => {
        let v = bytes[a.offset as usize] + bytes[b.offset as usize];
        bytes[dest.offset as usize] = v;
      }
      Op::Error => {
        panic!("Encountered error opcode")
      }
    }
    i += 1;
  }
}

fn main() {
  let contents =
    fs::read_to_string("examples/core.gen")
    .expect("Something went wrong reading the file");
  println!("file contents:\n\n{}", contents);
  let ast = parse(contents);
  for (i, n) in ast.nodes.iter().enumerate() {
    let s = &ast.code[n.start..n.end];
    println!("  {}:\t{:?}, {:?}", i, s, children(n, &ast.child_indices));
  }
}

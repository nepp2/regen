
use crate::bytecode::{ByteCode, Op, OpContent};

unsafe fn interpret(bc : &ByteCode) {
  use OpContent::*;
  let mut i = 0;
  let mut bytes : Vec<u8> = Vec::with_capacity(2048);
  loop {
    match &bc.ops[i].content {
      LiteralU8(val) => {

      }
      Set{ src_ptr, dest_ptr, size} => {

      }
      CJump{ cond, then_block, else_block } => {

      }
      Jump(block) => {

      }
      Exit => {

      }
    }
    // match &bc.ops[i] {
    //   Op::LiteralU8(dest, value) => {
    //     bytes[dest.offset as usize] = *value;
    //   }
    //   Op::Set{ src, dest, size} => {
    //     let src = (src.offset as usize)..((src.offset + size) as usize);
    //     bytes.as_mut_slice().copy_within(src, dest.offset as usize)
    //   }
    //   Op::CJump{ cond, then_block, else_block } => {
    //     i = if bytes[cond.offset as usize] == 0
    //       { *else_block } else { *then_block };
    //     continue;
    //   }
    //   Op::Add{a, b, dest} => {
    //     let v = bytes[a.offset as usize] + bytes[b.offset as usize];
    //     bytes[dest.offset as usize] = v;
    //   }
    //   Op::Error => {
    //     panic!("Encountered error opcode")
    //   }
    // }
    i += 1;
  }
}
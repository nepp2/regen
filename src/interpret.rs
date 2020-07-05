
use crate::symbols::Symbol;
use crate::bytecode::{ByteCode, Op, Expr, Function};

use std::collections::HashMap;

type Env = HashMap<Symbol, u64>;

pub fn interpret(bc : &ByteCode, env : &mut Env) {
  println!("Entering interpreter");
  // program counter
  let mut pc = 0;
  // registers
  let mut reg = [0 ; 2048];
  loop {
    match &bc.ops[pc] {
      Op::Expr(r, e) => {
        let v = match e {
          Expr::Symbol(sym) =>
            sym.0 as u64,
          Expr::LiteralU64(val) =>
            *val as u64,
          Expr::Add(a, b) =>
            reg[a.0] + reg[b.0],
        };
        reg[r.0] = v;
      }
      Op::Set(var, val) => {
        reg[var.0] = reg[val.0];
      }
      Op::CJump{ cond, then_block, else_block } => {
        if reg[cond.0] != 0 {
          pc = bc.blocks[then_block.0].start_op;
        }
        else {
          pc = bc.blocks[else_block.0].start_op;
        }
        continue;
      }
      Op::Jump(block) => {
        pc = bc.blocks[block.0].start_op;
        continue;
      }
      Op::Call(fun) => {
        let symbol = Symbol(reg[fun.0] as usize);
        let f = *env.get(&symbol).unwrap() as *const Function;
        let bc = unsafe { &(*f).bytecode };
        interpret(bc, env);
      }
      Op::Debug(s) => {
        println!("debug: {}", s);
      }
      Op::Exit => {
        println!("Exiting interpreter");
        return;
      }
    }
    pc += 1;
  }
}
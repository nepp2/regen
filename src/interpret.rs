
use crate::{symbols, parse, bytecode, env, ffi, compile};

use env::{Env, new_env};
use symbols::{to_symbol, SymbolTable};
use parse::Node;
use bytecode::{
  BytecodeFunction, Op, Expr, RegIndex, Operator, Alignment,
};

use std::fs;
use std::path::Path;

pub type CompileExpression = fn(env : &Env, code : &str, fun : Node) -> BytecodeFunction;

pub fn interpret(st : SymbolTable, n : &Node, code : &str) {
  let mut env = new_env(st);
  let mut stack = [0 ; 2048];
  let mut shadow_stack = vec![];
  for &c in n.children {
    let f = compile::compile_expr_to_function(&env, code, c);
    shadow_stack.clear();
    shadow_stack.push(
      Frame {
        pc: 0, sbp: 0,
        f: (&f) as *const BytecodeFunction,
        return_addr: 0,
      }
    );
    interpreter_loop(&mut stack, &mut shadow_stack, &mut env);
  }

  if let Some(&v) = env.values.get(&to_symbol(st, "main")) {
    let f = v as *const BytecodeFunction;
    shadow_stack.clear();
    shadow_stack.push(
      Frame { pc: 0, sbp: 0, f, return_addr: 0 });
    interpreter_loop(&mut stack, &mut shadow_stack, &mut env);
  }
  else {
    println!("No main function found.");
  }
}
  
struct Frame {
  /// program counter
  pc : usize,

  /// the stack base pointer (index of the beginning of the current stack frame)
  sbp : usize,

  /// function pointer
  f : *const BytecodeFunction,

  /// return address
  return_addr : usize,
}

fn reg(sbp : usize, i : RegIndex) -> usize {
  i.0 as usize + sbp
}

fn interpreter_loop(stack : &mut [u64], shadow_stack : &mut Vec<Frame>, env : &mut Env) {
  use Alignment::*;
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.ops[frame.pc];
      match op {
        Op::Expr(r, e) => {
          use Operator::*;
          let r = reg(sbp, r);
          match e {
            Expr::Def(sym) =>
              if let Some(&f) = env.values.get(&sym) {
                stack[r] = f;
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            Expr::LiteralU64(val) => {
              stack[r] = val as u64;
            }
            Expr::BinaryOp(op, a, b) => {
              let a = stack[reg(sbp, a)];
              let b  = stack[reg(sbp, b)];
              stack[r] = match op {
               Add => a + b,
               Sub => a - b,
               Mul => a * b,
               Div => a / b,
               Rem => a % b,
               Eq => (a == b) as u64,
               LT => (a < b) as u64,
               GT => (a > b) as u64,
               LTE => (a <= b) as u64,
               GTE => (a >= b) as u64,
               _ => panic!("op not binary"),
              };
            }
            Expr::UnaryOp(Ref, a) => {
              stack[r] = ((&stack[reg(sbp, a)]) as *const u64) as u64;
            }
            Expr::UnaryOp(op, a) => {
              let a = stack[reg(sbp, a)];
              stack[r] = match op {
                Sub => panic!("signed types not yet supported"),
                Load(U64) => unsafe { *(a as *const u64) },
                Load(U32) => unsafe { *(a as *const u32) as u64 },
                Load(U16) => unsafe { *(a as *const u16) as u64 },
                Load(U8) => unsafe { *(a as *const u8) as u64 },
                _ => panic!("op not unary"),
              };
            }
            Expr::Invoke(f) => {
              let fun_address = stack[reg(sbp, f)];
              let f = unsafe { &*(fun_address as *const BytecodeFunction) };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              shadow_stack.push(frame);
              // set the new frame
              frame = Frame{
                pc: 0,
                sbp: sbp + fun.registers,
                f,
                return_addr: r,
              };
              break;
            }
            Expr::InvokeC(f, arg_count) => {
              let fptr = stack[reg(sbp, f)] as *const ();
              let args = {
                let offset = sbp + fun.registers;
                &stack[offset..(offset + arg_count)]
              };
              stack[r] = unsafe {
                ffi::call_c_function(fptr, args)
              };
            }
          };
        }
        Op::Set(var, val) => {
          stack[reg(sbp, var)] = stack[reg(sbp, val)];
        }
        Op::Store{ alignment, pointer, value } => {
          let p = stack[reg(sbp, pointer)];
          let v = stack[reg(sbp, value)];
          match alignment {
            U64 => unsafe{ *(p as *mut u64) = v},
            U32 => unsafe{ *(p as *mut u32) = v as u32},
            U16 => unsafe{ *(p as *mut u16) = v as u16},
            U8 => unsafe{ *(p as *mut u8) = v as u8},
          }
        }
        Op::SetReturn(val) => {
          stack[frame.return_addr] = stack[reg(sbp, val)];
        }
        Op::CJump{ cond, then_seq, else_seq } => {
          let v = stack[reg(sbp, cond)];
          if v != 0 {
            frame.pc = fun.sequence_info[then_seq.0].start_op;
          }
          else {
            frame.pc = fun.sequence_info[else_seq.0].start_op;
          }
          continue;
        }
        Op::Jump(seq) => {
          frame.pc = fun.sequence_info[seq.0].start_op;
          continue;
        }
        Op::Arg{ index, value } => {
          let index = frame.sbp + fun.registers + (index as usize);
          stack[index] = stack[reg(sbp, value)];

        }
        Op::Debug(sym, r) => {
          println!("{}: {}", sym, stack[reg(sbp, r)]);
        }
        Op::Return => {
          if let Some(prev_frame) = shadow_stack.pop() {
            frame = prev_frame;
            break;
          }
          else {
            break 'outer;
          }
        }
      }
      frame.pc += 1;
    }
  }
  shadow_stack.push(frame);
}

pub fn run_file(path : impl AsRef<Path>) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let st = symbols::symbol_table();
  let ast = parse::parse(&code);
  interpret(st, &ast, &code);
}

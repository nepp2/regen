
use crate::{symbols, parse, bytecode, env, ffi};

use env::Env;
use symbols::to_symbol;
use parse::{
  Node, code_segment, node_shape, NodeShape::Command
};
use bytecode::{
  BytecodeFunction, Op, Expr, RegIndex, BinOp,
};

pub extern "C" fn c_add(a : u64, b : u64) -> u64 {
  a + b
}

pub type CompileFunction = fn(env : &Env, code : &str, fun : Node) -> BytecodeFunction;

pub fn interpret(n : &Node, code : &str, f : CompileFunction) {
  println!("Entering interpreter");
  let mut env = Env::new();

  env.insert(to_symbol("c_add"), c_add as u64);

  for &c in n.children {
    if let Command("def", [name, value]) = node_shape(&c, code) {
      let name = code_segment(code, *name);
      let function = f(&env, code, *value);
      let value =
        Box::into_raw(Box::new(function)) as u64;
      env.insert(to_symbol(name), value);
    }
  }

  if let Some(&v) = env.get(&to_symbol("main")) {
    let f = v as *const BytecodeFunction;
    let mut stack = [0 ; 2048];
    let mut shadow_stack = vec![Frame { pc: 0, sbp: 0, f }];
    interpreter_loop(&mut stack, &mut shadow_stack, &mut env);
    println!("Execution of main function complete.");
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
}

fn reg(sbp : usize, i : RegIndex) -> usize {
  i.0 as usize + sbp
}

fn interpreter_loop(stack : &mut [u64], shadow_stack : &mut Vec<Frame>, env : &mut Env) {
  let mut frame = shadow_stack.pop().unwrap();
  let mut return_addr = 0;
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.ops[frame.pc];
      // TODO: println!(">>> {}:   {}", frame.pc, op);
      match op {
        Op::Expr(r, e) => {
          let r = reg(sbp, r);
          match e {
            Expr::Def(sym) =>
              if let Some(&f) = env.get(&sym) {
                stack[r] = f;
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            Expr::LiteralU64(val) => {
              stack[r] = val as u64;
            }
            Expr::BinOp(op, a, b) => {
              let a = stack[reg(sbp, a)];
              let b  = stack[reg(sbp, b)];
              stack[r] = match op {
               BinOp::Add => a + b,
               BinOp::Sub => a - b,
               BinOp::Mul => a * b,
               BinOp::Div => a / b,
               BinOp::Rem => a % b,
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
              frame = Frame{ pc: 0, sbp: sbp + fun.registers, f };
              // set the return address
              return_addr = r;
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
        Op::SetReturn(val) => {
          stack[return_addr] = stack[reg(sbp, val)];
        }
        Op::CJump{ cond, then_seq, else_seq } => {
          let v = stack[reg(sbp, cond)];
          if v != 0 {
            frame.pc = fun.sequences[then_seq.0].start_op;
          }
          else {
            frame.pc = fun.sequences[else_seq.0].start_op;
          }
          continue;
        }
        Op::Jump(seq) => {
          frame.pc = fun.sequences[seq.0].start_op;
          continue;
        }
        Op::Arg{ index, value } => {
          let index = frame.sbp + fun.registers + (index as usize);
          stack[index] = stack[reg(sbp, value)];

        }
        Op::Debug(r) => {
          println!("debug: {}", stack[reg(sbp, r)]);
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

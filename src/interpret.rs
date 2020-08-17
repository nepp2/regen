
use crate::{symbols, parse, bytecode, env, ffi};

use env::Env;
use symbols::{to_symbol, SymbolTable, Symbol};
use parse::{
  Node, code_segment, node_shape, NodeShape::Command
};
use bytecode::{
  BytecodeFunction, Op, Expr, RegIndex, BinOp,
};

use std::fs;
use std::path::Path;

pub extern "C" fn c_add(a : u64, b : u64) -> u64 {
  a + b
}

pub extern "C" fn env_insert(env : &mut Env, sym : Symbol, value : u64) {
  env.values.insert(sym, value);
}

pub extern "C" fn env_get(env : &Env, sym : Symbol) -> u64 {
  env.values[&sym]
}

pub extern "C" fn print_symbol(sym : Symbol) {
  println!("symbol: {}", sym)
}

pub type CompileFunction = fn(env : &Env, code : &str, fun : Node) -> BytecodeFunction;

fn new_env(st : SymbolTable) -> Box<Env> {
  let env_ptr = Box::into_raw(Box::new(Env {
    values: Default::default(),
    st
  }));
  let mut env = unsafe { Box::from_raw(env_ptr) };
  env.values.insert(to_symbol(st, "c_add"), c_add as u64);
  env.values.insert(to_symbol(st, "env"), env_ptr as u64);
  env.values.insert(to_symbol(st, "env_insert"), env_insert as u64);
  env.values.insert(to_symbol(st, "env_get"), env_get as u64);
  env.values.insert(to_symbol(st, "print_symbol"), print_symbol as u64);
  env
}

pub fn interpret(st : SymbolTable, n : &Node, code : &str, f : CompileFunction) {
  let mut env = new_env(st);

  for &c in n.children {
    if let Command("def", [name, value]) = node_shape(&c, code) {
      let name = code_segment(code, *name);
      let function = f(&env, code, *value);
      // TODO: add debug flag to enable this
      // println!("function {}:", name);
      // println!("{}", function);
      let value =
        Box::into_raw(Box::new(function)) as u64;
      env.values.insert(to_symbol(st, name), value);
    }
  }

  if let Some(&v) = env.values.get(&to_symbol(st, "main")) {
    let f = v as *const BytecodeFunction;
    let mut stack = [0 ; 2048];
    let mut shadow_stack = vec![
      Frame { pc: 0, sbp: 0, f, return_addr: 0 }
    ];
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
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.ops[frame.pc];
      match op {
        Op::Expr(r, e) => {
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
            Expr::BinOp(op, a, b) => {
              let a = stack[reg(sbp, a)];
              let b  = stack[reg(sbp, b)];
              stack[r] = match op {
               BinOp::Add => a + b,
               BinOp::Sub => a - b,
               BinOp::Mul => a * b,
               BinOp::Div => a / b,
               BinOp::Rem => a % b,
               BinOp::Eq => (a == b) as u64,
               BinOp::LT => (a < b) as u64,
               BinOp::GT => (a > b) as u64,
               BinOp::LTE => (a <= b) as u64,
               BinOp::GTE => (a >= b) as u64,
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

pub fn run_file(path : impl AsRef<Path>, f : CompileFunction) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  let st = symbols::symbol_table();
  let ast = parse::parse(&code);
  interpret(st, &ast, &code, f);
}

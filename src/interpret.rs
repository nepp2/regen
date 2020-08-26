
use crate::{symbols, parse, bytecode, env, ffi, compile};

use env::{Env, new_env};
use symbols::SymbolTable;
use parse::Node;
use bytecode::{
  BytecodeFunction, Op, Expr, Var, Operator, ByteWidth,
};

use std::fs;
use std::path::Path;

pub type CompileExpression = fn(env : &Env, code : &str, fun : Node) -> BytecodeFunction;

pub fn interpret(st : SymbolTable, n : &Node, code : &str) {
  let mut env = new_env(st);
  let mut stack = [0 as u8 ; 16384];
  let mut shadow_stack = vec![];
  let stack_ptr = StackPtr {
    mem: &mut stack[0] as *mut u8,
    byte_pos: 0,
    max_bytes: stack.len() as u32,
  };
  for &c in n.children {
    let f = compile::compile_expr_to_function(&env, code, c);
    shadow_stack.clear();
    shadow_stack.push(
      Frame {
        pc: 0,
        sbp: stack_ptr,
        f: (&f) as *const BytecodeFunction,
        return_addr: stack_ptr.byte_offset(0),
      }
    );
    interpreter_loop(&mut shadow_stack, &mut env);
  }

  if let Some(v) = env.get_str("main") {
    let f = v.value as *const BytecodeFunction;
    shadow_stack.clear();
    shadow_stack.push(
      Frame { pc: 0,
        sbp: stack_ptr,
        f,
        return_addr: stack_ptr.byte_offset(0),
      });
    interpreter_loop(&mut shadow_stack, &mut env);
  }
  else {
    println!("No main function found.");
  }
}
  
struct Frame {
  /// program counter
  pc : usize,

  /// the stack base pointer (byte index of the beginning of the current stack frame)
  sbp : StackPtr,

  /// function pointer
  f : *const BytecodeFunction,

  /// return address
  return_addr : *mut u64,
}

fn reg(sbp : usize, i : Var) -> usize {
  i.byte_offset + sbp
}

fn interpreter_loop(shadow_stack : &mut Vec<Frame>, env : &mut Env) {
  use ByteWidth::*;
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.ops[frame.pc];
      match op {
        Op::Expr(var, e) => {
          use Operator::*;
          match e {
            Expr::Def(sym) =>
              if let Some(f) = env.get(sym) {
                set_var(sbp, var, f.value);
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            Expr::LiteralU64(val) => {
              set_var(sbp, var, val as u64);
            }
            Expr::BinaryOp(op, a, b) => {
              let a = get_var(sbp, a);
              let b  = get_var(sbp, b);
              let val = match op {
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
              set_var(sbp, var, val);
            }
            Expr::UnaryOp(Ref, a) => {
              set_var(sbp, var, var_addr(sbp, a) as u64);
            }
            Expr::UnaryOp(op, a) => {
              let a = get_var(sbp, a);
              let val = match op {
                Sub => panic!("signed types not yet supported"),
                Load(U64) => unsafe { *(a as *const u64) },
                Load(U32) => unsafe { *(a as *const u32) as u64 },
                Load(U16) => unsafe { *(a as *const u16) as u64 },
                Load(U8) => unsafe { *(a as *const u8) as u64 },
                _ => panic!("op not unary"),
              };
              set_var(sbp, var, val);
            }
            Expr::Invoke(f) => {
              let fun_address = get_var(sbp, f);
              let f = unsafe { &*(fun_address as *const BytecodeFunction) };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              shadow_stack.push(frame);
              // set the new frame
              frame = Frame{
                pc: 0,
                sbp: sbp.advance_frame(fun.frame_bytes),
                f,
                return_addr: var_addr(sbp, var),
              };
              break;
            }
            Expr::InvokeC(f, arg_count) => {
              let fptr = get_var(sbp, f) as *const ();
              let args = unsafe {
                let data = sbp.advance_frame(fun.frame_bytes).u64_offset(0);
                std::slice::from_raw_parts(data, arg_count)
              };
              let val = unsafe { ffi::call_c_function(fptr, args) };
              set_var(sbp, var, val);
            }
          };
        }
        Op::Set(var, val) => {
          set_var(sbp, var, get_var(sbp, val));
        }
        Op::Store{ byte_width, pointer, value } => {
          let p = get_var(sbp, pointer);
          let v = get_var(sbp, value);
          match byte_width {
            U64 => unsafe{ *(p as *mut u64) = v},
            U32 => unsafe{ *(p as *mut u32) = v as u32},
            U16 => unsafe{ *(p as *mut u16) = v as u16},
            U8 => unsafe{ *(p as *mut u8) = v as u8},
          }
        }
        Op::SetReturn(val) => {
          let v = get_var(sbp, val);
          unsafe {
            *frame.return_addr = v;
          }
        }
        Op::CJump{ cond, then_seq, else_seq } => {
          let v = get_var(sbp, cond);
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
          let arg_ptr = sbp.advance_frame(fun.frame_bytes).u64_offset(index as usize);
          unsafe {
            *arg_ptr = get_var(sbp, value);
          }
        }
        Op::Debug(sym, r) => {
          println!("{}: {}", sym, get_var(sbp, r));
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

#[derive(Copy, Clone)]
struct StackPtr {
  mem : *mut u8,
  byte_pos : u32,
  max_bytes : u32,
}

impl StackPtr {
  fn advance_frame(self, frame_bytes : usize) -> Self {
    let new_byte_pos = self.byte_pos + (frame_bytes as u32);
    if new_byte_pos >= self.max_bytes {
      panic!("exceeded stack")
    }
    StackPtr { mem: self.mem, byte_pos: new_byte_pos, max_bytes: self.max_bytes }
  }

  fn byte_offset(self, byte_offset : usize) -> *mut u64 {
    unsafe { self.mem.add(self.byte_pos as usize + byte_offset) as *mut u64 }
  }

  fn u64_offset(self, index : usize) -> *mut u64 {
    unsafe { (self.mem.add(self.byte_pos as usize) as *mut u64).add(index) }
  }
}

fn var_addr(sbp : StackPtr, v : Var) -> *mut u64 {
  sbp.byte_offset(v.byte_offset)
}

fn set_var(sbp : StackPtr, v : Var, val : u64) {
  unsafe {
    *var_addr(sbp, v) = val;
  }
}

fn get_var(sbp : StackPtr, v : Var) -> u64 {
  unsafe { *var_addr(sbp, v) }
}

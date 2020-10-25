/// Bytecode interpreter
///
/// Receives a sexp node tree and compiles/interprets the top level
/// nodes one at a time.

use crate::{parse, bytecode, env, ffi, compile, types};

use env::Env;
use parse::Node;
use bytecode::{
  Op, Expr, FrameVar, Operator,
};
use compile::Function;

pub type CompileExpression = fn(env : &Env, fun : Node) -> Function;

pub fn interpret_function(f : *const Function, args : &[u64], env : Env) -> u64 {
  let mut stack = [0 as u8 ; 16384];
  let stack_ptr = StackPtr {
    mem: &mut stack[0] as *mut u8,
    byte_pos: 0,
    max_bytes: stack.len() as u32,
  };
  let stack_args = unsafe {
    std::slice::from_raw_parts_mut(stack_ptr.mem as *mut u64, args.len())
  };
  stack_args.copy_from_slice(args);
  let mut shadow_stack = vec![
    Frame {
      pc: 0,
      sbp: stack_ptr,
      f,
      return_addr: stack_ptr.byte_offset(0),
    }
  ];
  interpreter_loop(&mut shadow_stack, env);
  unsafe {
    let t = (*f).t;
    let fun = types::type_as_function(&t).unwrap();
    if fun.returns.size_of == 8 {
      *(stack_ptr.mem as *mut u64)
    }
    else {
      0
    }
  }
}

pub fn interpret_node(n : Node, env : Env) -> u64 {
  let f = compile::compile_expr_to_function(env, n);
  interpret_function(&f, &[], env)
}

pub fn interpret_file(code : &str, env : Env) {
  let n = parse::parse(env.st, &code);
  for &c in n.children() {
    interpret_node(c, env);
  }
}
  
struct Frame {
  /// program counter
  pc : usize,

  /// the stack base pointer (byte index of the beginning of the current stack frame)
  sbp : StackPtr,

  /// function pointer
  f : *const Function,

  /// return address
  return_addr : *mut u64,
}

fn reg(sbp : usize, i : FrameVar) -> usize {
  i.byte_offset as usize + sbp
}

fn interpreter_loop(shadow_stack : &mut Vec<Frame>, env : Env) {
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.bc.ops[frame.pc];
      match op {
        Op::Expr(var, e) => {
          use Operator::*;
          match e {
            Expr::Def(sym) => {
              if let Some(f) = env.get(sym) {
                set_var(sbp, var, f.value);
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
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
                Not => !(a != 0) as u64,
                _ => panic!("op not unary"),
              };
              set_var(sbp, var, val);
            }
            Expr::Invoke(f) => {
              let fun_address = get_var(sbp, f);
              let f = unsafe { &*(fun_address as *const Function) };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              shadow_stack.push(frame);
              // set the new frame
              frame = Frame{
                pc: 0,
                sbp: sbp.advance_frame(fun.bc.frame_bytes),
                f,
                return_addr: var_addr(sbp, var),
              };
              break;
            }
            Expr::InvokeC(f, arg_count) => {
              let fptr = get_var(sbp, f) as *const ();
              let args = unsafe {
                let data = sbp.advance_frame(fun.bc.frame_bytes).u64_offset(0);
                std::slice::from_raw_parts(data, arg_count)
              };
              let val = unsafe { ffi::call_c_function(fptr, args) };
              set_var(sbp, var, val);
            }
            Expr::Load{ bytes, ptr } => {
              let p = get_var(sbp, ptr);
              let vaddr = var_addr(sbp, var);
              unsafe { load(p as *mut (), vaddr, bytes as usize) }
            }
          };
        }
        Op::Set(dest, src) => {
          let src_addr = var_addr(sbp, src);
          let dest_addr = var_addr(sbp, dest);
          unsafe { store(src_addr, dest_addr as *mut (), src.bytes as usize) }
        }
        Op::Store{ byte_width, pointer, value } => {
          let dest = get_var(sbp, pointer);
          let src_register = var_addr(sbp, value);
          unsafe { store(src_register, dest as *mut (), byte_width as usize) }
        }
        Op::CJump{ cond, then_seq, else_seq } => {
          let v = get_var(sbp, cond);
          if v != 0 {
            frame.pc = fun.bc.sequence_info[then_seq.0].start_op;
          }
          else {
            frame.pc = fun.bc.sequence_info[else_seq.0].start_op;
          }
          continue;
        }
        Op::Jump(seq) => {
          frame.pc = fun.bc.sequence_info[seq.0].start_op;
          continue;
        }
        Op::Arg{ byte_offset, value } => {
          let arg_ptr = sbp.advance_frame(fun.bc.frame_bytes).byte_offset(byte_offset);
          unsafe {
            *arg_ptr = get_var(sbp, value);
          }
        }
        Op::Debug(sym, r, t) => {
          println!("{}: {} of type {}", sym, get_var(sbp, r), t);
        }
        Op::Return(val) => {
          if let Some(val) = val {
            let src_addr = var_addr(sbp, val);
            unsafe {
              store(src_addr, frame.return_addr as *mut (), val.bytes as usize);
            }
          }
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

#[derive(Copy, Clone)]
struct StackPtr {
  mem : *mut u8,
  byte_pos : u32,
  max_bytes : u32,
}

impl StackPtr {
  fn advance_frame(self, frame_bytes : u64) -> Self {
    let new_byte_pos = self.byte_pos + (frame_bytes as u32);
    if new_byte_pos >= self.max_bytes {
      panic!("exceeded stack")
    }
    StackPtr { mem: self.mem, byte_pos: new_byte_pos, max_bytes: self.max_bytes }
  }

  fn byte_offset(self, byte_offset : u64) -> *mut u64 {
    unsafe { self.mem.add((self.byte_pos as u64 + byte_offset) as usize) as *mut u64 }
  }

  fn u64_offset(self, index : usize) -> *mut u64 {
    unsafe { (self.mem.add(self.byte_pos as usize) as *mut u64).add(index) }
  }
}

fn var_addr(sbp : StackPtr, v : FrameVar) -> *mut u64 {
  sbp.byte_offset(v.byte_offset as u64)
}

fn set_var(sbp : StackPtr, v : FrameVar, val : u64) {
  unsafe {
    *var_addr(sbp, v) = val;
  }
}

fn get_var(sbp : StackPtr, v : FrameVar) -> u64 {
  unsafe { *var_addr(sbp, v) }
}

unsafe fn store(src_register : *mut u64, dest : *mut (), byte_width : usize) {
  match byte_width {
    8 => *(dest as *mut u64) = *src_register,
    4 => *(dest as *mut u32) = *src_register as u32,
    2 => *(dest as *mut u16) = *src_register as u16,
    1 => *(dest as *mut u8) = *src_register as u8,
    _ =>
      std::ptr::copy_nonoverlapping(
        src_register as *mut u8, // src
        dest as *mut u8, // dest
        byte_width as usize),
  }
}

unsafe fn load(src : *mut (), dest_register : *mut u64, byte_width : usize) {
  match byte_width {
    8 => *dest_register = *(src as *mut u64),
    4 => *dest_register = *(src as *mut u32) as u64,
    2 => *dest_register = *(src as *mut u16) as u64,
    1 => *dest_register = *(src as *mut u8) as u64,
    _ =>
      std::ptr::copy_nonoverlapping(
        src as *mut u8, // src
        dest_register as *mut u8, // dest
        byte_width as usize),
  }
}
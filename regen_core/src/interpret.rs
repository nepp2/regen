/// Bytecode interpreter
///
/// Receives a sexp node tree and compiles/interprets the top level
/// nodes one at a time.

use crate::{parse, bytecode, env, ffi, compile, types, debug, perm_alloc};

use env::Env;
use parse::Node;
use bytecode::{
  Instr, Expr, RegId, Operator, LocalId,
};
use compile::Function;
use perm_alloc::PermSlice;
use types::TupleInfo;

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
      return_addr: stack_ptr.to_raw_ptr(0),
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
  return_addr : *mut (),
}

fn interpreter_loop(shadow_stack : &mut Vec<Frame>, env : Env) {
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    loop {
      let instr = fun.bc.instrs[frame.pc];
      match instr {
        Instr::Expr(var, e) => {
          use Operator::*;
          match e {
            Expr::Local(l) => {
              let local_offset = frame.local_byte_offset(l);
              let v = frame.sbp.to_raw_ptr(local_offset) as u64;
              frame.set_reg(var, &v);
            }
            Expr::Def(sym) => {
              if let Some(f) = env.get(sym) {
                frame.set_reg(var, &f.value);
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            }
            Expr::Init(t, field_vals) => {
              let tuple = types::type_as_tuple(&t).expect("expected tuple");
              frame.initialise_tuple(var, tuple, field_vals)
            }
            Expr::LiteralU64(val) => {
              frame.set_reg(var, &val);
            }
            Expr::BinaryOp(op, a, b) => {
              let a : u64 = frame.get_reg(a);
              let b : u64  = frame.get_reg(b);
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
              frame.set_reg(var, &val);
            }
            Expr::UnaryOp(op, a) => {
              let a : u64 = frame.get_reg(a);
              let val = match op {
                Sub => panic!("signed types not yet supported"),
                Not => !(a != 0) as u64,
                _ => panic!("op not unary"),
              };
              frame.set_reg(var, &val);
            }
            Expr::Invoke(f, args) => {
              frame.push_args(args);
              let fun_address : *const Function = frame.get_reg(f);
              let f = unsafe { &*fun_address };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              let sbp = frame.sbp.advance_bytes(fun.bc.frame_bytes);
              let return_addr = frame.reg_addr(var);
              shadow_stack.push(frame);
              // set the new frame
              frame = Frame {
                pc: 0,
                sbp, f,
                return_addr,
              };
              break;
            }
            Expr::InvokeC(f, args) => {
              let fptr : *const () = frame.get_reg(f);
              frame.push_args(args);
              let args = unsafe {
                let data = frame.sbp.advance_bytes(fun.bc.frame_bytes).u64_offset(0);
                std::slice::from_raw_parts(data, args.len)
              };
              let val = unsafe { ffi::call_c_function(fptr, args) };
              frame.set_reg(var, &val);
            }
            Expr::Load(ptr) => {
              let p : *mut () = frame.get_reg(ptr);
              let vaddr = frame.reg_addr(var);
              let bytes = frame.reg_sizeof(var) as usize;
              unsafe { store(p as *mut (), vaddr, bytes) }
            }
          };
        }
        Instr::Store{ pointer, value } => {
          let dest : *mut () = frame.get_reg(pointer);
          let src_register = frame.reg_addr(value);
          let byte_width = frame.reg_sizeof(value) as usize;
          unsafe { store(src_register, dest, byte_width) }
        }
        Instr::CJump{ cond, then_seq, else_seq } => {
          let v : u64 = frame.get_reg(cond);
          if v != 0 {
            frame.pc = fun.bc.sequence_info[then_seq.0].start_instruction;
          }
          else {
            frame.pc = fun.bc.sequence_info[else_seq.0].start_instruction;
          }
          continue;
        }
        Instr::Jump(seq) => {
          frame.pc = fun.bc.sequence_info[seq.0].start_instruction;
          continue;
        }
        Instr::Debug(sym, r, t) => {
          let p = frame.reg_addr(r);
          println!("{}: {}", sym, debug::display(p as *const (), t));
        }
        Instr::Return(val) => {
          if let Some(val) = val {
            let src_addr = frame.reg_addr(val);
            let byte_width = frame.reg_sizeof(val) as usize;
            unsafe {
              store(src_addr, frame.return_addr as *mut (), byte_width);
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
  fn advance_bytes(self, frame_bytes : u64) -> Self {
    let new_byte_pos = self.byte_pos + (frame_bytes as u32);
    if new_byte_pos >= self.max_bytes {
      panic!("exceeded stack")
    }
    StackPtr { mem: self.mem, byte_pos: new_byte_pos, max_bytes: self.max_bytes }
  }

  fn to_raw_ptr(self, byte_offset : u64) -> *mut () {
    unsafe { self.mem.add((self.byte_pos as u64 + byte_offset) as usize) as *mut () }
  }

  fn u64_offset(self, index : usize) -> *mut u64 {
    unsafe { (self.mem.add(self.byte_pos as usize) as *mut u64).add(index) }
  }
}

impl Frame {
  
  fn initialise_tuple(&self, r : RegId, t : &TupleInfo, field_vals : PermSlice<RegId>) {
    let addr = self.reg_addr(r) as *mut u8;
    for i in 0..field_vals.len() {
      let offset = t.field_offsets[i];
      let val_addr = self.reg_addr(field_vals[i]);
      let byte_width = self.reg_sizeof(r);
      unsafe {
        let dest = addr.add(offset as usize);
        store(val_addr, dest as *mut (), byte_width as usize);
      }
    }
  }

  fn push_args(&self, args : PermSlice<RegId>) {
    let args_ptr =
        self.sbp.advance_bytes(self.fun().bc.frame_bytes);
    let mut byte_offset = 0;
    for &a in args {
      let arg_ptr = args_ptr.to_raw_ptr(byte_offset);
      let byte_width = types::round_up_multiple(self.reg_sizeof(a), 8);
      unsafe { store(self.reg_addr(a), arg_ptr, byte_width as usize) };
      byte_offset += byte_width;
    }
  }

  fn fun(&self) -> &Function {
    unsafe { &*self.f }
  }

  fn reg_byte_offset(&self, r : RegId) -> u64 {
    self.fun().bc.registers[r.id].byte_offset as u64
  }

  fn reg_sizeof(&self, r : RegId) -> u64 {
    self.fun().bc.registers[r.id].t.size_of as u64
  }

  fn local_byte_offset(&self, l : LocalId) -> u64 {
    self.fun().bc.locals[l.id].byte_offset as u64
  }

  fn reg_addr(&self, r : RegId) -> *mut () {
    let byte_offset = self.reg_byte_offset(r);
    self.sbp.to_raw_ptr(byte_offset) as *mut ()
  }

  fn set_reg<T>(&self, dest : RegId, val : &T) {
    let src = val as *const T;
    unsafe {
      store(
        src as *const (),
        self.reg_addr(dest),
        self.reg_sizeof(dest) as usize);
    }
  }
  
  fn get_reg<T : Copy>(&self, r : RegId) -> T {
    unsafe { *(self.reg_addr(r) as *const T) }
  }
}

unsafe fn store(src : *const (), dest : *mut (), byte_width : usize) {
  match byte_width {
    8 => *(dest as *mut u64) = *(src as *const u64),
    4 => *(dest as *mut u32) = *(src as *const u32),
    2 => *(dest as *mut u16) = *(src as *const u16),
    1 => *(dest as *mut u8) = *(src as *const u8),
    _ =>
      std::ptr::copy_nonoverlapping(
        src as *const u8, // src
        dest as *mut u8, // dest
        byte_width as usize),
  }
}

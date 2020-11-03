/// Bytecode interpreter
///
/// Receives a sexp node tree and compiles/interprets the top level
/// nodes one at a time.

use crate::{parse, bytecode, env, ffi, compile, types, debug, perm_alloc};

use env::Env;
use parse::Node;
use bytecode::{
  Instr, Expr, Operator, LocalId,
};
use compile::Function;
use perm_alloc::PermSlice;
use types::TypeHandle;

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
            Expr::LocalAddr(l) => {
              let local_offset = frame.local_byte_offset(l);
              let v = frame.sbp.to_raw_ptr(local_offset) as u64;
              frame.set_local(var, &v);
            }
            Expr::Def(sym) => {
              if let Some(f) = env.get(sym) {
                frame.set_local(var, &f.value);
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            }
            Expr::Init(t, field_vals) => {
              let info = types::type_as_struct(&t).expect("expected struct");
              frame.initialise_struct(var, info.field_offsets, field_vals)
            }
            Expr::FieldIndex { struct_addr, index } => {
              let ptr_type = frame.local_type(struct_addr);
              let t = types::deref_pointer_type(&ptr_type).unwrap();
              let info = types::type_as_struct(&t).expect("expected struct");
              let field_offset = info.field_offsets[index as usize];
              let tuple_ptr : *const u8 = frame.get_local(struct_addr);
              let field_ptr = unsafe { tuple_ptr.add(field_offset as usize) };
              frame.set_local(var, &field_ptr);
            }
            Expr::LiteralU64(val) => {
              frame.set_local(var, &val);
            }
            Expr::BinaryOp(op, a, b) => {
              let a : u64 = frame.get_local(a);
              let b : u64  = frame.get_local(b);
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
              frame.set_local(var, &val);
            }
            Expr::UnaryOp(op, a) => {
              let a : u64 = frame.get_local(a);
              let val = match op {
                Sub => panic!("signed types not yet supported"),
                Not => !(a != 0) as u64,
                _ => panic!("op not unary"),
              };
              frame.set_local(var, &val);
            }
            Expr::Invoke(f, args) => {
              frame.push_args(args);
              let fun_address : *const Function = frame.get_local(f);
              let f = unsafe { &*fun_address };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              let sbp = frame.sbp.advance_bytes(fun.bc.frame_bytes);
              let return_addr = frame.local_addr(var);
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
              let fptr : *const () = frame.get_local(f);
              frame.push_args(args);
              let args = unsafe {
                let data = frame.sbp.advance_bytes(fun.bc.frame_bytes).u64_offset(0);
                std::slice::from_raw_parts(data, args.len)
              };
              let val = unsafe { ffi::call_c_function(fptr, args) };
              frame.set_local(var, &val);
            }
            Expr::Load(ptr) => {
              let p : *mut () = frame.get_local(ptr);
              let vaddr = frame.local_addr(var);
              let bytes = frame.local_sizeof(var) as usize;
              unsafe { store(p as *mut (), vaddr, bytes) }
            }
          };
        }
        Instr::Store{ pointer, value } => {
          let dest : *mut () = frame.get_local(pointer);
          let src_localister = frame.local_addr(value);
          let byte_width = frame.local_sizeof(value) as usize;
          unsafe { store(src_localister, dest, byte_width) }
        }
        Instr::CJump{ cond, then_seq, else_seq } => {
          let v : u64 = frame.get_local(cond);
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
        Instr::Debug(sym, l, t) => {
          let p = frame.local_addr(l);
          println!("{}: {}", sym, debug::display(p as *const (), t));
        }
        Instr::Return(val) => {
          if let Some(val) = val {
            let src_addr = frame.local_addr(val);
            let byte_width = frame.local_sizeof(val) as usize;
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
  
  fn initialise_struct(&self, l : LocalId, field_offsets : PermSlice<u64>, field_vals : PermSlice<LocalId>) {
    let addr = self.local_addr(l) as *mut u8;
    for i in 0..field_vals.len() {
      let offset = field_offsets[i];
      let val_addr = self.local_addr(field_vals[i]);
      let byte_width = self.local_sizeof(l);
      unsafe {
        let dest = addr.add(offset as usize);
        store(val_addr, dest as *mut (), byte_width as usize);
      }
    }
  }

  fn push_args(&self, args : PermSlice<LocalId>) {
    let args_ptr =
        self.sbp.advance_bytes(self.fun().bc.frame_bytes);
    let mut byte_offset = 0;
    for &a in args {
      let arg_ptr = args_ptr.to_raw_ptr(byte_offset);
      let byte_width = types::round_up_multiple(self.local_sizeof(a), 8);
      unsafe { store(self.local_addr(a), arg_ptr, byte_width as usize) };
      byte_offset += byte_width;
    }
  }

  fn fun(&self) -> &Function {
    unsafe { &*self.f }
  }

  fn local_byte_offset(&self, l : LocalId) -> u64 {
    self.fun().bc.locals[l.id].byte_offset as u64
  }

  fn local_type(&self, l : LocalId) -> TypeHandle {
    self.fun().bc.locals[l.id].t
  }

  fn local_sizeof(&self, l : LocalId) -> u64 {
    self.fun().bc.locals[l.id].t.size_of as u64
  }

  fn local_addr(&self, l : LocalId) -> *mut () {
    let byte_offset = self.local_byte_offset(l);
    self.sbp.to_raw_ptr(byte_offset) as *mut ()
  }

  fn set_local<T>(&self, dest : LocalId, val : &T) {
    let src = val as *const T;
    unsafe {
      store(
        src as *const (),
        self.local_addr(dest),
        self.local_sizeof(dest) as usize);
    }
  }
  
  fn get_local<T : Copy>(&self, l : LocalId) -> T {
    unsafe { *(self.local_addr(l) as *const T) }
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

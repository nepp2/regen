/// Bytecode interpreter
///
/// Receives a sexp node tree and compiles/interprets the top level
/// nodes one at a time.

use crate::{bytecode, compile, debug, env, ffi_ccall, parse, perm_alloc, sexp, symbols::Symbol, types};

use env::{Env};
use sexp::{Node, NodeShape};
use parse::Expr;
use bytecode::{
  Instr, InstrExpr, Operator, LocalHandle,
};
use compile::Function;
use perm_alloc::SlicePtr;
use types::{TypeHandle, Primitive};

pub type CompileExpression = fn(env : &Env, fun : Node) -> Function;

pub fn interpret_function(f : *const Function, args : &[u64], env : Env, return_addr : Option<*mut ()>) {
  let mut stack = [0 as u8 ; 16384];
  let mut return_offset = 0;
  let return_addr = {
    if let Some(ptr) = return_addr {
      ptr
    }
    else {
      // if the caller doesn't supply a return address,
      // make room for the return value on the stack
      let t = unsafe { (*f).t };
      let fun = types::type_as_function(&t).unwrap();
      return_offset += fun.returns.size_of;
      &mut stack[0] as *mut u8 as *mut ()
    }
  };
  let stack_ptr = StackPtr {
    mem: &mut stack[return_offset as usize] as *mut u8,
    byte_pos: 0,
    max_bytes: stack.len() as u32,
  };
  let stack_args = unsafe {
    std::slice::from_raw_parts_mut(stack_ptr.mem as *mut u64, args.len())
  };
  stack_args.copy_from_slice(args);
  // unsafe {
  //   let t = (*f).t;
  //   let fun = types::type_as_function(&t).unwrap();
  //   if fun.returns.size_of == 8 {
  //     *(stack_ptr.mem as *mut u64)
  //   }
  //   else {
  //     0
  //   }
  // }
  let mut shadow_stack = vec![
    Frame {
      pc: 0,
      sbp: stack_ptr,
      f,
      return_addr,
    }
  ];
  interpreter_loop(&mut shadow_stack, env);
}

pub fn interpret_def_expr(name : Symbol, expr : Expr, env : Env) {
  let f = compile::compile_expr_to_function(env, expr);
  let def_type = types::type_as_function(&f.t).unwrap().returns;
  let (def_ptr, region) = env::env_alloc_global(env, name, def_type);
  env::set_active_region(env, region);
  interpret_function(&f, &[], env, Some(def_ptr));
}

pub fn interpret_def_node(name : Symbol, expr : Node, env : Env) {
  let e = parse::parse_to_expr(env.st, expr);
  interpret_def_expr(name, e, env)
}

pub fn interpret_node(expr : Node, env : Env) {
  let e = parse::parse_to_expr(env.st, expr);
  let f = compile::compile_expr_to_function(env, e);
  interpret_function(&f, &[], env, None);
}

pub fn interpret_file(module_name : &str, code : &str, env : Env) {
  let n = sexp::sexp_list(env.st, module_name, code);
  for c in n.children() {
    match sexp::node_shape(c) {
      NodeShape::Command("def", [name, expr]) => {
        let name = name.as_symbol();
        interpret_def_node(name, *expr, env);
      }
      _ => {
        interpret_node(*c, env);
      }
    }
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

macro_rules! binop {
  ($frame:ident, $var:ident, $op:ident, $a:expr, $b:expr, $t:ty) => {{
    let var = $var;
    let a : $t = $frame.get_local($a);
    let b : $t = $frame.get_local($b);
    match $op {
      Add => $frame.set_local(var, &(a + b)),
      Sub => $frame.set_local(var, &(a - b)),
      Mul => $frame.set_local(var, &(a * b)),
      Div => $frame.set_local(var, &(a / b)),
      Rem => $frame.set_local(var, &(a % b)),
      Eq => $frame.set_local(var, &(a == b)),
      LT => $frame.set_local(var, &(a < b)),
      GT => $frame.set_local(var, &(a > b)),
      LTE => $frame.set_local(var, &(a <= b)),
      GTE => $frame.set_local(var, &(a >= b)),
      _ => panic!("op not binary"),
    };
  }};
}

fn interpreter_loop(shadow_stack : &mut Vec<Frame>, env : Env) {
  let mut frame = shadow_stack.pop().unwrap();
  'outer: loop {
    let fun = unsafe { &*frame.f };
    loop {
      let instr = fun.bc.instrs[frame.pc];
      // println!("   {}", instr);
      match instr {
        Instr::Expr(var, e) => {
          use Operator::*;
          use InstrExpr::*;
          match e {
            LocalAddr(l) => {
              let v = frame.local_addr(l) as u64;
              frame.set_local(var, &v);
            }
            Def(sym) => {
              if let Some(entry) = env::get_entry(&env, sym) {
                frame.set_local(var, &entry.ptr);
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            }
            Array(element_vals) => {
              let info = types::type_as_array(&var.t).expect("expected array");
              frame.initialise_array(var, info.inner, element_vals)
            }
            Init(field_vals) => {
              let info = types::type_as_struct(&var.t).expect("expected struct");
              frame.initialise_struct(var, info.field_offsets, field_vals)
            }
            ZeroInit => {
              unsafe {
                std::ptr::write_bytes(frame.local_addr(var), 0, var.t.size_of as usize);
              }
            }
            FieldIndex { struct_addr, index } => {
              let ptr_type = struct_addr.t;
              let t = types::deref_pointer_type(ptr_type).unwrap();
              let info = types::type_as_struct(&t).expect("expected struct");
              let field_offset = info.field_offsets[index as usize];
              let tuple_ptr : *const u8 = frame.get_local(struct_addr);
              let field_ptr = unsafe { tuple_ptr.add(field_offset as usize) };
              frame.set_local(var, &field_ptr);
            }
            LiteralU64(val) => {
              frame.set_local(var, &val);
            }
            Literal(_t, p) => {
              frame.set_local(var, &p);
            }
            BinaryOp(op, a, b) => {
              use Primitive::*;
              let t = types::type_as_primitive(&a.t).unwrap();
              match t {
                U64 => binop!(frame, var, op, a, b, u64),
                U32 => binop!(frame, var, op, a, b, u32),
                U16 => binop!(frame, var, op, a, b, u16),
                U8 => binop!(frame, var, op, a, b, u8),
                _ => panic!("binary op {} not supported for operands of type {}", op, a.t),
              }
              
            }
            UnaryOp(op, a) => {
              let a : u64 = frame.get_local(a);
              let val = match op {
                Sub => panic!("signed types not yet supported"),
                Not => !(a != 0) as u64,
                _ => panic!("op not unary"),
              };
              frame.set_local(var, &val);
            }
            Invoke(f, args) => {
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
            InvokeC(f, args) => {
              let fptr : *const () = frame.get_local(f);
              frame.push_args(args);
              let args = unsafe {
                let data = frame.sbp.advance_bytes(fun.bc.frame_bytes).u64_offset(0);
                std::slice::from_raw_parts(data, args.len)
              };
              let val = unsafe { ffi_ccall::call_c_function(fptr, args) };
              // TODO: the line below is incorrect for void types, and probably also
              // for any types that isn't exactly 64 bits wide. The arguments passed are
              // also suspect; args smaller than 64 bits may be passed incorrectly.
              unsafe {
                let v = (&val as *const u64) as *const ();
                cast_and_store(v, frame.local_addr(var), env.c.u64_tag, var.t);
              }
            }
            Load(ptr) => {
              let p : *mut () = frame.get_local(ptr);
              let vaddr = frame.local_addr(var);
              unsafe { store(p as *mut (), vaddr, var.t.size_of) }
            }
            PtrOffset{ ptr, offset } => {
              let sizeof =
                types::deref_pointer_type(var.t)
                .unwrap().size_of;
              let offset : u64 = frame.get_local(offset);
              let p : u64 = frame.get_local(ptr);
              frame.set_local(var, &(p + (sizeof * offset)));
            }
            Cast(v) => {
              frame.cast(v, var);
            }
          };
        }
        Instr::Store{ pointer, value } => {
          let dest : *mut () = frame.get_local(pointer);
          let value_addr = frame.local_addr(value);
          let byte_width = value.t.size_of;
          unsafe { store(value_addr, dest, byte_width) }
        }
        Instr::CJump{ cond, then_seq, else_seq } => {
          let v : bool = frame.get_local(cond);
          if v {
            frame.pc = then_seq.start_instruction;
          }
          else {
            frame.pc = else_seq.start_instruction;
          }
          continue;
        }
        Instr::Jump(seq) => {
          frame.pc = seq.start_instruction;
          continue;
        }
        Instr::Debug(sym, l, t) => {
          let p = frame.local_addr(l);
          println!("debug {}: {}", sym, debug::display(p as *const (), t));
        }
        Instr::Return(val) => {
          if let Some(val) = val {
            let src_addr = frame.local_addr(val);
            unsafe {
              store(src_addr, frame.return_addr as *mut (), val.t.size_of);
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

  fn initialise_array(&self, l : LocalHandle, element_type : TypeHandle, element_vals : SlicePtr<LocalHandle>) {
    let addr = self.local_addr(l) as *mut u8;
    let byte_width = element_type.size_of;
    for i in 0..element_vals.len() {
      let e = element_vals[i];
      let val_addr = self.local_addr(e);
      unsafe {
        let dest = addr.add((byte_width as usize) * i);
        store(val_addr, dest as *mut (), byte_width);
      }
    }
  }

  fn initialise_struct(&self, l : LocalHandle, field_offsets : SlicePtr<u64>, field_vals : SlicePtr<LocalHandle>) {
    let addr = self.local_addr(l) as *mut u8;
    for i in 0..field_vals.len() {
      let offset = field_offsets[i];
      let fv = field_vals[i];
      let val_addr = self.local_addr(fv);
      unsafe {
        let dest = addr.add(offset as usize);
        store(val_addr, dest as *mut (), fv.t.size_of);
      }
    }
  }

  fn push_args(&self, args : SlicePtr<LocalHandle>) {
    let args_ptr =
        self.sbp.advance_bytes(self.fun().bc.frame_bytes);
    let mut byte_offset = 0;
    for &a in args {
      let arg_ptr = args_ptr.to_raw_ptr(byte_offset);
      let byte_width = types::round_up_multiple(a.t.size_of, 8);
      unsafe { store(self.local_addr(a), arg_ptr, byte_width) };
      byte_offset += byte_width;
    }
  }

  fn fun(&self) -> &Function {
    unsafe { &*self.f }
  }

  fn local_addr(&self, l : LocalHandle) -> *mut () {
    self.sbp.to_raw_ptr(l.byte_offset) as *mut ()
  }

  fn set_local<T>(&self, dest : LocalHandle, val : &T) {
    let src = val as *const T;
    let dest_size = dest.t.size_of;
    let val_size = std::mem::size_of::<T>() as u64;
    if dest_size != val_size {
      panic!("can't assign {} byte value to {} byte location", val_size, dest_size)
    }
    unsafe {
      store(
        src as *const (),
        self.local_addr(dest),
        dest_size);
    }
  }

  fn cast(&self, src : LocalHandle, dest : LocalHandle) {
    let src_addr = self.local_addr(src);
    let dest_addr = self.local_addr(dest);
    unsafe {
      cast_and_store(src_addr, dest_addr, src.t, dest.t);
    }
  }
  
  fn get_local<T : Copy>(&self, l : LocalHandle) -> T {
    let val_size = std::mem::size_of::<T>() as u64;
    if l.t.size_of != val_size {
      panic!("can't get {} byte value from {} byte location", val_size, l.t.size_of)
    }
    unsafe { *(self.local_addr(l) as *const T) }
  }
}

unsafe fn cast_and_store(src : *const (), dest : *mut (), src_type : TypeHandle, dest_type : TypeHandle) {
  if src_type.size_of == dest_type.size_of {
    store(
      src,
      dest,
      dest_type.size_of);
  }
  else {
    use Primitive::*;
    let src_prim = types::type_as_primitive(&src_type).unwrap();
    let dest_prim = types::type_as_primitive(&dest_type).unwrap();
    match (src_prim, dest_prim) {
      (U64, U32) => *(dest as *mut u64) = (*(src as *mut u32)) as u64,
      (U64, U16) => *(dest as *mut u64) = (*(src as *mut u16)) as u64,
      (U64, U8) => *(dest as *mut u64) = (*(src as *mut u8)) as u64,
      (U64, Bool) => *(dest as *mut u64) = (*(src as *mut u8)) as u64,
      (U64, Void) => (),

      (U32, U64) => *(dest as *mut u32) = (*(src as *mut u64)) as u32,
      (U16, U64) => *(dest as *mut u16) = (*(src as *mut u64)) as u16,
      (U8, U64) => *(dest as *mut u8) = (*(src as *mut u64)) as u8,
      _ => panic!("unsupported cast between {} and {}", src_type, dest_type)
    }
  }
}

unsafe fn store(src : *const (), dest : *mut (), byte_width : u64) {
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

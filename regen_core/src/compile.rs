/// Compiles core language into bytecode

use crate::{
  bytecode, sexp, symbols, env, perm_alloc,
  types, parse,
};

use bytecode::{
  SequenceHandle, SequenceInfo, InstrExpr, FunctionBytecode,
  Instr, Operator, LocalHandle, LocalInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use perm_alloc::{Perm, perm_slice_from_vec, perm_slice, perm};

use symbols::Symbol;
use env::Env;
use sexp::{ Node, NodeLiteral };
use parse::{Expr, ExprContent};

struct LabelledExpr {
  name : Symbol,
  entry_seq: SequenceHandle,
  exit_seq: SequenceHandle,
}

pub struct Function {
  pub bc : FunctionBytecode,
  pub t : TypeHandle,
}

/// Function builder
struct Builder {
  bc : FunctionBytecode,
  
  /// local variables in scope
  scoped_vars : Vec<NamedVar>,
  
  /// the instruction sequence currently being built
  current_sequence : Option<SequenceHandle>,

  /// flags indicating which sequences have been finalised
  seq_completion : Vec<bool>,

  /// labels indicating the start and end of a labelled block expression
  label_stack : Vec<LabelledExpr>,

  /// the environment containing all visible symbols
  env : Env,
}

impl Builder {
  fn new(env : Env) -> Self {
    Builder {
      bc: FunctionBytecode {
        sequence_info: vec![],
        instrs: vec![],
        args: 0,
        locals: vec![],
        frame_bytes: 0,
      },
      scoped_vars: vec![],
      seq_completion: vec![],
      current_sequence: None,
      label_stack: vec![],
      env,
    }
  }
}

#[derive(Copy, Clone)]
enum RefType {
  /// A locator register contains a pointer to a value.
  /// Must be derefenced into value register to be read from.
  /// Can be written to.
  Locator(LocalHandle),

  /// A value register can be read from directly, but can't be written to.
  Value(LocalHandle),
}

/// A reference can be used to obtain a value or its address. A reference may be:
///   * a var containing a value
///   * a var containing the address of a value
#[derive(Copy, Clone)]
pub struct Ref {
  ref_type : RefType,
  t : TypeHandle,
  mutable : bool,
}

impl Ref {

  fn from_var(local : Var) -> Self {
    Ref{ ref_type: RefType::Value(local.id), t: local.t, mutable: local.mutable }
  }

  fn get_address(&self, b : &mut Builder) -> Var {
    let t = types::pointer_type(self.t);
    let id = match self.ref_type {
      RefType::Locator(id) => id,
      RefType::Value(id) => {
        let e = InstrExpr::LocalAddr(id);
        let addr_id = new_local(b, None, t, self.mutable);
        b.bc.instrs.push(Instr::Expr(addr_id, e));
        addr_id
      }
    };
    Var { id, t: t, mutable: self.mutable }
  }
  
  fn to_var(&self, b : &mut Builder) -> Var {
    match self.ref_type {
      RefType::Locator(r) => {
        let e = InstrExpr::Load(r);
        push_expr(b, e, self.t)
      }
      RefType::Value(r) => Var {
        id: r,
        t: self.t,
        mutable: self.mutable,
      },
    }
  }
}

#[derive(Copy, Clone)]
struct Var {
  id : LocalHandle,
  t : TypeHandle,
  mutable : bool,
}

impl Var {
  fn to_ref(&self) -> Ref {
    Ref::from_var(*self)
  }
}

#[derive(Copy, Clone)]
struct NamedVar {
  name : Symbol,
  v : Var,
}

fn find_var_in_scope(b : &mut Builder, name : Symbol)
  -> Option<Var>
{
  let result =
    b.scoped_vars.iter().rev()
    .find(|&l| l.name == name).cloned();
  if let Some(nl) = result {
    return Some(nl.v);
  }
  None
}

fn new_local_variable(b : &mut Builder, name : Symbol, t : TypeHandle) -> Var {
  let id = new_local(b, Some(name), t, true);
  let v = Var { id, t: t, mutable: true };
  b.scoped_vars.push(NamedVar{ name, v });
  v
}

fn new_local(b : &mut Builder, name : Option<Symbol>, t : TypeHandle, mutable : bool) -> LocalHandle {
  let info = LocalInfo {
    index: b.bc.locals.len(),
    name,
    byte_offset: 0,
    t,
    mutable,
  };
  let lh = perm(info);
  b.bc.locals.push(lh);
  lh
}

fn new_var(b : &mut Builder, t : TypeHandle, mutable : bool) -> Var {
  let id = new_local(b, None, t, mutable);
  Var { id, t: t, mutable }
}

fn push_expr(b : &mut Builder, e : InstrExpr, t : TypeHandle) -> Var {
  let v = new_var(b, t, false);
  b.bc.instrs.push(Instr::Expr(v.id, e));
  return v;
}

fn pointer_to_locator(v : Var, mutable : bool) -> Ref {
  let inner = types::deref_pointer_type(v.t).expect("expected pointer type");
  Ref {
    ref_type: RefType::Locator(v.id),
    t: inner,
    mutable,
  }
}

fn find_label(b : &mut Builder, label : Symbol) -> &LabelledExpr {
  b.label_stack.iter().rev()
  .find(|l| l.name == label)
  .expect("label not found")
}

fn create_sequence(b : &mut Builder, name : &str) -> SequenceHandle {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(b.env.st, name);
  loop {
    let name_unique = b.bc.sequence_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(b.env.st, &format!("{}_{}", name, i));
  }
  let seq = perm(SequenceInfo {
    index: b.bc.sequence_info.len(),
    name: name_candidate,
    start_instruction: 0, num_instructions: 0,
  });
  b.bc.sequence_info.push(seq);
  b.seq_completion.push(false);
  seq
}

fn set_current_sequence(b : &mut Builder, mut sequence : SequenceHandle) {
  // complete the current sequence (if there is one)
  complete_sequence(b);
  // check that the sequence isn't done yet
  if b.seq_completion[sequence.index] {
    panic!("this sequence has already been completed");
  }
  b.current_sequence = Some(sequence);
  sequence.start_instruction = b.bc.instrs.len();
}

fn complete_sequence(b : &mut Builder) {
  if let Some(mut seq) = b.current_sequence {
    seq.num_instructions = b.bc.instrs.len() - seq.start_instruction;
    b.seq_completion[seq.index] = true;
    b.current_sequence = None;
  }
}

fn complete_function(mut b : Builder) -> FunctionBytecode {
  complete_sequence(&mut b);
  // check all sequences are complete
  if !b.seq_completion.iter().all(|x| *x) {
    panic!("not all basic sequences were completed")
  }
  // set byte offsets
  for l in b.bc.locals.as_mut_slice() {
    l.byte_offset = b.bc.frame_bytes;
    let bytes = types::round_up_multiple(l.t.size_of, 8);
    b.bc.frame_bytes += bytes;
  }
  b.bc
}

fn function_to_var(b : &mut Builder, f : Function) -> Var {
  let function_var = new_var(b, f.t, false);
  let f_addr = Box::into_raw(Box::new(f)) as u64;
  b.bc.instrs.push(Instr::Expr(function_var.id, InstrExpr::LiteralU64(f_addr)));
  function_var
}

fn compile_function_def(env: Env, args : &[(Symbol, Expr)], return_tag : Option<Expr>, body : Expr) -> Function {
  let mut b = Builder::new(env);
  b.bc.args = args.len();
  let mut arg_types = vec![];
  for &(name, tag) in args {
    let t = expr_to_type(&b, tag);
    new_local_variable(&mut b, name, t);
    arg_types.push(t);
  }
  let expected_return = return_tag.map(|e| expr_to_type(&mut b, e));
  compile_function(b, body, &arg_types, expected_return)
}

fn compile_function(
  mut b : Builder, body : Expr, arg_types : &[TypeHandle],
  expected_return : Option<TypeHandle>) -> Function
{
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);

  let mut return_type = b.env.c.void_tag;
  let result = compile_expr(&mut b, body);
  if let Some(r) = result {
    let v = r.to_var(&mut b);
    return_type = v.t;
    b.bc.instrs.push(Instr::Return(Some(v.id)));
  }
  else {
    b.bc.instrs.push(Instr::Return(None));
  }
  if let Some(et) = expected_return {
    if et != return_type {
      panic!("expected return type {}, found {}", et, return_type);
    }
  }
  let t = types::function_type(&arg_types, return_type);
  let f = complete_function(b);
  // for n in body { println!("{}", n) }
  // println!("{}", f);
  Function { bc: f, t }
}

/// Compile basic imperative language into bytecode
pub fn compile_expr_to_function(env: Env, root : Expr) -> Function {
  compile_function(Builder::new(env), root, &[], None)
}

fn compile_if_else(b : &mut Builder, cond_expr : Expr, then_expr : Expr, else_expr : Expr) -> Option<Ref> {
  let mut result_var = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_var(b, cond_expr).id;
  assert_type(cond_expr.n, cond.t, b.env.c.bool_tag);
  b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result = compile_expr(b, then_expr);
  if let Some(v) = then_result {
    let nv = new_var(b, v.t, true).to_ref();
    result_var = Some(nv);
    let v = v.to_var(b);
    compile_assignment(b, nv, v);
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result = compile_expr(b, else_expr);
  if let Some(l) = result_var {
    let v = else_result.expect("expected block expression").to_var(b);
    compile_assignment(b, l, v);
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  result_var
}

fn compile_assignment(b : &mut Builder, dest : Ref, value : Var) {
  if dest.t.size_of != value.t.size_of {
    panic!("types don't match")
  }
  if !dest.mutable {
    panic!("can't assign to this value")
  }
  let pointer = dest.get_address(b).id;
  b.bc.instrs.push(Instr::Store {
    pointer,
    value: value.id,
  });
}

/// dereferences any pointer types encountered
fn compile_and_fully_deref(b : &mut Builder, e : Expr) -> Ref {
  let mut r = compile_expr_to_ref(b, e);
  while r.t.kind == Kind::Pointer {
    let v = r.to_var(b);
    r = pointer_to_locator(v, r.mutable);
  }
  r
}

fn compile_expr_to_ref(b : &mut Builder, e : Expr) -> Ref {
  if let Some(r) = compile_expr(b, e) {
    r
  }
  else {
    panic!("expected value, found none, at node {} ({})", e.n, e.n.loc);
  }
}

fn compile_expr_to_var(b : &mut Builder, e : Expr) -> Var {
  let r = compile_expr_to_ref(b, e);
  r.to_var(b)
}

fn assert_type(n : Node, t : TypeHandle, expected : TypeHandle) {
  if t != expected {
    panic!("expected {}, found {} at ({})", expected, t, n.loc)
  }
}

fn compile_cast(b : &mut Builder, v : Var, t : TypeHandle) -> Var {
  fn ptr_or_prim(t : TypeHandle) -> bool {
    t.kind == Kind::Pointer || t.kind == Kind::Primitive
  }
  if t.size_of != v.t.size_of {
    if !(ptr_or_prim(t) && ptr_or_prim(v.t)) {
      panic!("cannot cast from {} to {}", v.t, t);
    }
  }
  push_expr(b, InstrExpr::Cast(v.id), t)
}

fn compile_expr(b : &mut Builder, e : Expr) -> Option<Ref> {
  match e.content {
    // return
    ExprContent::Return(None) => {
      b.bc.instrs.push(Instr::Return(None));
      None
    }
    // return value
    ExprContent::Return(Some(v)) => {
      let var = compile_expr_to_var(b, v);
      b.bc.instrs.push(Instr::Return(Some(var.id)));
      None
    }
    // break
    ExprContent::Break(None) => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // break to label
    ExprContent::Break(Some(label)) => {
      let break_to = find_label(b, label).exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // repeat
    ExprContent::Repeat(None) => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.bc.instrs.push(Instr::Jump(loop_back_to));
      None
    }
    // repeat to label
    ExprContent::Repeat(Some(label)) => {
      let repeat_to = find_label(b, label).entry_seq;
      b.bc.instrs.push(Instr::Jump(repeat_to));
      None
    }
    // boolean
    ExprContent::LiteralBool(v) => {
      let e = InstrExpr::LiteralU64(if v { 1 } else { 0 });
      let v = push_expr(b, e, b.env.c.u64_tag);
      Some(compile_cast(b, v, b.env.c.bool_tag).to_ref())
    }
    // local reference
    ExprContent::LocalRef(sym) => {
      // Look for variable in local scope
      if let Some(v) = find_var_in_scope(b, sym) {
        Some(v.to_ref())
      }
      else {
        panic!("local var not found in codegen!")
      }
    }
    // global reference
    ExprContent::GlobalRef(sym) => {
      if let Some(tag) = env::get_global_type(b.env, sym) {
        let e = InstrExpr::Def(sym);
        let v = push_expr(b, e, types::pointer_type(tag));
        Some(pointer_to_locator(v, true))
      }
      else {
        panic!("symbol '{}' not defined ({})", sym, e.n.loc)
      }
    }
    // literal u64
    ExprContent::LiteralU64(v) => {
      let e = InstrExpr::LiteralU64(v);
      Some(push_expr(b, e, b.env.c.u64_tag).to_ref())
    }
    // literal string
    ExprContent::LiteralString(s) => {
      let t = types::pointer_type(b.env.c.string_tag);
      let e = InstrExpr::Literal(t, Perm::to_ptr(s) as *const ());
      let v = push_expr(b, e, t);
      Some(pointer_to_locator(v, false))
    }
    // literal void
    ExprContent::LiteralVoid => {
      None
    }
    // array
    ExprContent::ArrayInit(elements) => {
      let e = compile_expr_to_var(b, elements[0]);
      let element_type = e.t;
      let mut element_values = vec![e.id];
      for e in &elements[1..] {
        let v = compile_expr_to_var(b, *e);
        if v.t != element_type {
          panic!("expected element of type {} and found type {}, at {}",
            element_type, v.t, e.n.loc);
        }
        element_values.push(v.id);
      }
      let t = types::array_type(element_type, elements.len() as u64);
      let e = InstrExpr::Array(perm_slice_from_vec(element_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // array length
    ExprContent::ArrayAsSlice(array) => {
      let r = compile_expr_to_ref(b, array);
      let info = types::type_as_array(&r.t).expect("expected array");
      let array_ptr = r.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array_ptr, element_ptr_type);
      let len = compile_array_len(b, array);
      let e = InstrExpr::Init(perm_slice(&[ptr.id, len.id]));
      let t = types::slice_type(&b.env.c, b.env.st, info.inner);
      Some(push_expr(b, e, t).to_ref())
    }
    // array length
    ExprContent::ArrayLen(array) => {
      Some(compile_array_len(b, array).to_ref())
    }
    // array index
    ExprContent::ArrayIndex{ array , index } => {
      let v = compile_expr_to_ref(b, array);
      let info = if let Some(info) = types::type_as_array(&v.t) {
        info
      }
      else {
        panic!("expected array at ({})", array.n.loc)
      };
      let array_ptr = v.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array_ptr, element_ptr_type);
      let offset = compile_expr_to_var(b, index).id;
      let e = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ptr.t);
      Some(pointer_to_locator(ptr, v.mutable))
    }
    // ptr index
    ExprContent::PtrIndex{ ptr , index } => {
      let ptr = compile_expr_to_var(b, ptr);
      let offset = compile_expr_to_var(b, index).id;
      let e = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ptr.t);
      Some(pointer_to_locator(ptr, ptr.mutable))
    }
    // zero init
    ExprContent::ZeroInit(t) => {
      let t = expr_to_type(b, t);
      Some(push_expr(b, InstrExpr::ZeroInit, t).to_ref())      
    }
    // init
    ExprContent::StructInit(type_expr, field_vals) => {
      let t = expr_to_type(b, type_expr);
      let mut field_values = Vec::with_capacity(field_vals.len);
      for f in field_vals {
        // TODO: check types
        let aaa = ();
        field_values.push(compile_expr_to_var(b, *f).id);
      }
      let e = InstrExpr::Init(perm_slice_from_vec(field_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // field deref
    ExprContent::FieldIndex { structure, field_name } => {
      let struct_ref = compile_and_fully_deref(b, structure);
      let struct_addr = struct_ref.get_address(b).id;
      let info = types::type_as_struct(&struct_ref.t).expect("expected struct");
      let i = {
        let sym = field_name.as_symbol();
        info.field_names.as_slice().iter()
          .position(|n| *n == sym)
          .expect("no such field") as u64
      };
      let field_type = info.field_types[i as usize];
      let e = InstrExpr::FieldIndex{ struct_addr, index: i };
      let v = push_expr(b, e, types::pointer_type(field_type));
      Some(pointer_to_locator(v, struct_ref.mutable))
    }
    // quotation
    ExprContent::Quote(quoted) => {
      Some(compile_quote(b, quoted).to_ref())
    }
    // def
    ExprContent::DefMarker { name, initialiser } => {
      // TODO: just ignores the name. this is a bit odd.
      compile_expr(b, initialiser)
    }
    // fun
    ExprContent::Fun { args, ret, body } => {
      let f = compile_function_def(b.env, args.as_slice(), ret, body);
      let v = function_to_var(b, f);
      Some(v.to_ref())      
    }
    // set var
    ExprContent::Set { dest, value } => {
      let dest = compile_expr_to_ref(b, dest);
      let value = compile_expr_to_var(b, value);
      compile_assignment(b, dest, value);
      None
    }
    // let
    ExprContent::Let(var_name, value) => {
      // TODO: this generates redundant bytecode. If the value is a
      // locator, it should just generate a load, rather than a load and a store.
      let name = var_name.as_symbol();
      // evaluate the expression
      let value = compile_expr_to_var(b, value);
      let local_var = new_local_variable(b, name, value.t);
      compile_assignment(b, local_var.to_ref(), value);
      None
    }
    // if then
    ExprContent::IfElse { cond, then_expr, else_expr: None } => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond_var = compile_expr_to_var(b, cond).id;
      assert_type(cond.n, cond_var.t, b.env.c.bool_tag);
      b.bc.instrs.push(Instr::CJump{ cond: cond_var, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_expr(b, then_expr);
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    ExprContent::IfElse { cond, then_expr, else_expr: Some(else_expr) } => {
      compile_if_else(b, cond, then_expr, else_expr)
    }
    // label expression
    ExprContent::LabelledBlock(label, body) => {
      let entry_seq = create_sequence(b, "block_entry");
      let exit_seq = create_sequence(b, "block_exit");
      b.bc.instrs.push(Instr::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledExpr{ name: label, entry_seq, exit_seq }
      );
      let result = compile_expr(b, body);
      b.label_stack.pop();
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result.map(|x| x)
    }
    ExprContent::Do(exprs) => {
      let mut v = None;
      let num_locals = b.scoped_vars.len();
      for &c in exprs {
        v = compile_expr(b, c);
      }
      b.scoped_vars.drain(num_locals..).for_each(|_| ());
      let result = v.map(|v| v.to_var(b)); // Can't pass ref out of block scope
      result.map(|x| x.to_ref())
    }
    // debug
    ExprContent::Debug(v) => {
      let local = compile_expr_to_var(b, v);
      b.bc.instrs.push(Instr::Debug(v.n, local.id, local.t));
      None
    }
    // symbol
    ExprContent::Sym(s) => {
      let e = InstrExpr::LiteralU64(s.as_u64());
      return Some(push_expr(b, e, b.env.c.u64_tag).to_ref());
    }
    // typeof
    ExprContent::TypeOf(v) => {
      // TODO: it's weird to codegen the expression when we only need its type
      let var = compile_expr_to_var(b, v);
      let e = InstrExpr::LiteralU64(Perm::to_u64(var.t));
      return Some(push_expr(b, e, b.env.c.type_tag).to_ref());
    }
    // deref
    ExprContent::Deref(pointer) => {
      let ptr = compile_expr_to_var(b, pointer);
      Some(pointer_to_locator(ptr, true))
    }
    // ref
    ExprContent::GetAddress(locator) => {
      let local = compile_expr_to_ref(b, locator);
      return Some(local.get_address(b).to_ref())
    }
    // cast
    ExprContent::Cast { value, to_type } => {
      let t = expr_to_type(b, to_type);
      let v = compile_expr_to_var(b, value);
      Some(compile_cast(b, v, t).to_ref())
    }
    // instrinsic op
    ExprContent::InstrinicOp(op, args) => {
      Some(compile_intrinic_op(b, e, op, args.as_slice()).to_ref())
    }
    // Call
    ExprContent::Call(function_expr, arg_exprs) => {
      Some(compile_function_call(b, e, function_expr, arg_exprs.as_slice()).to_ref())
    }
    // types
    ExprContent::PtrType(_) => {
      Some(compile_type_expr(b, e).to_ref())
    }
    // function type
    ExprContent::FnType { .. } => {
      Some(compile_type_expr(b, e).to_ref())
    }
    // c function type
    ExprContent::CFunType { .. } => {
      Some(compile_type_expr(b, e).to_ref())
    }
    // array type
    ExprContent::SizedArrayType { .. } => {
      Some(compile_type_expr(b, e).to_ref())
    }
    // struct type
    ExprContent::StructType(_) => {
      Some(compile_type_expr(b, e).to_ref())
    }
  }
}

fn compile_literal(b : &mut Builder, l : NodeLiteral) -> Ref {
  match l {
    NodeLiteral::U64(v) => {
      let e = InstrExpr::LiteralU64(v);
      push_expr(b, e, b.env.c.u64_tag).to_ref()
    }
    NodeLiteral::String(s) => {
      let t = types::pointer_type(b.env.c.string_tag);
      let e = InstrExpr::Literal(t, Perm::to_ptr(s) as *const ());
      let v = push_expr(b, e, t);
      pointer_to_locator(v, false)
    }
  }
}

fn compile_array_len(b : &mut Builder, array : Expr) -> Var {
  let r = compile_expr_to_ref(b, array);
  let info = types::type_as_array(&r.t).expect("expected array");
  let e = InstrExpr::LiteralU64(info.length);
  push_expr(b, e, b.env.c.u64_tag)
}

fn symbol_to_type(b : &Builder, s : Symbol) -> Option<TypeHandle> {
  env::get_global_value(b.env, s, b.env.c.type_tag)
}

fn eval_literal_u64(env : Env, e : Expr) -> u64 {
  if let ExprContent::LiteralU64(v) = e.content {
    return v;
  }
  if let ExprContent::GlobalRef(sym) = e.content {
    if let Some(v) = env::get_global_value(env, sym, env.c.u64_tag) {
      return v;
    }
  }
  panic!("{} is not literal u64 at ({})", e.n, e.n.loc)
}

fn compile_type_expr(b : &mut Builder, e : Expr) -> Var {
  let type_tag = expr_to_type(b, e);
  let e = InstrExpr::LiteralU64(Perm::to_u64(type_tag));
  push_expr(b, e, b.env.c.type_tag)
}

fn expr_to_type(b: &Builder, e : Expr) -> TypeHandle {
  if let Some(t) = try_expr_to_type(b, e) {
    t
  }
  else {
    panic!("{} is not valid type, at ({})", e.n, e.n.loc);
  }
}

fn try_expr_to_type(b: &Builder, e : Expr) -> Option<TypeHandle> {
  let t = match e.content {
    ExprContent::GlobalRef(sym) => {
      symbol_to_type(b, sym).expect("expected type")
    }
    // pointer type
    ExprContent::PtrType(inner_type) => {
      let inner = expr_to_type(b, inner_type);
      types::pointer_type(inner)
    }
    // function type
    ExprContent::FnType { args, ret } => {
      let arg_types : Vec<TypeHandle> =
        args.as_slice().iter().map(|a| expr_to_type(b, a.tag)).collect();
      let returns = expr_to_type(b, ret);
      types::function_type(&arg_types, returns)
    }
    // c function type
    ExprContent::CFunType { args, ret } => {
      let arg_types : Vec<TypeHandle> =
        args.as_slice().iter().map(|a| expr_to_type(b, a.tag)).collect();
      let returns = expr_to_type(b, ret);
      types::c_function_type(&arg_types, returns)
    }
    // array type
    ExprContent::SizedArrayType { element_type, length } => {
      let element = expr_to_type(b, element_type);
      let size = eval_literal_u64(b.env, length);
      types::array_type(element, size)
    }
    // struct type
    ExprContent::StructType(fields) => {
      let mut field_names = vec![];
      let mut field_types = vec![];
      for a in fields {
        if let Some(name) = a.name {
          field_names.push(name.as_symbol());
        }
        field_types.push(expr_to_type(b, a.tag));
      }
      types::struct_type(
        perm_slice_from_vec(field_names),
        perm_slice_from_vec(field_types))
    }
    _ => {
      return None;
    },
  };
  Some(t)
}

fn compile_symbol_reference(b : &mut Builder, node : Node) -> Ref {
  let sym = node.as_symbol();
  // Look for variable in local scope
  if let Some(r) = find_var_in_scope(b, sym) {
    return r.to_ref();
  }
  // Assume global
  if let Some(tag) = env::get_global_type(b.env, sym) {
    let e = InstrExpr::Def(sym);
    let v = push_expr(b, e, types::pointer_type(tag));
    return pointer_to_locator(v, true);
  }
  panic!("symbol '{}' not defined ({})", sym, node.loc)
}

fn compile_function_call(b : &mut Builder, e : Expr, function : Expr, args : &[Expr]) -> Var {
  let f = compile_expr_to_var(b, function);
  let info = if let Some(i) = types::type_as_function(&f.t) {
    i
  }
  else {
    panic!("expected function, found {} expression '{}' at ({})",
      f.t, function.n, function.n.loc);
  };
  if args.len() != info.args.len() {
    panic!("expected {} args, found {}, at ({})",
      info.args.len(), args.len(), e.n.loc);
  }
  let mut arg_values = vec![];
  for i in 0..args.len() {
    let arg = args[i];
    let v = compile_expr_to_var(b, arg);
    if info.args[i] != v.t  {
      panic!("expected arg of type {}, found type {}, at ({})", info.args[i], v.t, arg.n.loc);
    }
    if info.c_function && v.t.size_of > 8 {
      panic!("types passed to a C function must be 64 bits wide or less; found type {} of width {} as ({})",
        v.t, v.t.size_of, arg.n.loc);
    }
    arg_values.push(v.id);
  }
  let e = {
    if info.c_function {
      InstrExpr::InvokeC(f.id, perm_slice_from_vec(arg_values))
    }
    else {
      InstrExpr::Invoke(f.id, perm_slice_from_vec(arg_values))
    }
  };
  return push_expr(b, e, info.returns);
}

fn str_to_operator(s : &str) -> Option<Operator> {
  use Operator::*;
  let op = match s {
    "+" => Add,
    "-" => Sub,
    "*" => Mul,
    "/" => Div,
    "%" => Rem,
    "==" => Eq,
    "<" => LT,
    ">" => GT,
    "<=" => LTE,
    ">=" => GTE,
    "!" => Not,
    _ => return None,
  };
  Some(op)
}

fn binary_op_type(c : &CoreTypes, op : Operator, a : TypeHandle, b : TypeHandle) -> Option<TypeHandle> {
  use Operator::*;
  if a == b {
    match op {
      Add | Sub | Mul | Div | Rem | BitwiseAnd | BitwiseOr => {
        if types::is_number(a) {
          return Some(a);
        }
      }
      Eq => {
        if types::is_number(a) || types::is_bool(a) {
          return Some(c.bool_tag);
        }
      }
      LT | GT | LTE | GTE => {
        if types::is_number(a) {
          return Some(c.bool_tag);
        }
      }
      Not | BitwiseNot => (),
    }
  }
  None
}

fn unary_op_type(c : &CoreTypes, op : Operator, t : TypeHandle) -> Option<TypeHandle> {
  use Operator::*;
  if op == Not && t == c.bool_tag {
    return Some(c.bool_tag);
  }
  if op == Sub || op == BitwiseNot {
    if types::is_number(t) {
      return Some(t);
    }
  }
  None
}

fn compile_intrinic_op(b : &mut Builder, e : Expr, op : Operator, args : &[Expr]) -> Var {
  if let [v1, v2] = args {
    let v1 = compile_expr_to_var(b, *v1);
    let v2 = compile_expr_to_var(b, *v2);
    if let Some(t) = binary_op_type(&b.env.c, op, v1.t, v2.t) {
      let e = InstrExpr::BinaryOp(op, v1.id, v2.id);
      return push_expr(b, e, t);
    }
    panic!("no binary op {} for types {} and {} at ({})",
      op, v1.t, v2.t, e.n.loc)
  }
  if let [v1] = args {
    let v1 = compile_expr_to_var(b, *v1);
    if let Some(t) = unary_op_type(&b.env.c, op, v1.t) {
      let e = InstrExpr::UnaryOp(op, v1.id);
      return push_expr(b, e, t);
    }
    panic!("no unary op {} for type {} at ({})",
      op, v1.t, e.n.loc)
  }
  panic!("incorrect number of args to operator {} at ({})", op, e.n.loc)
}

fn compile_quote(b : &mut Builder, quoted : Node) -> Var {
  let e = InstrExpr::LiteralU64(Perm::to_ptr(quoted) as u64);
  let node_tag = b.env.c.node_tag;
  push_expr(b, e, node_tag)
}

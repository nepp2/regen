
/// Compiles core language into bytecode

use crate::{bytecode, env::{CellId, CellUid, CellValue, Env}, hotload::{CellResolver, CellStatus}, parse, perm_alloc, symbols::{self, SymbolTable}, types};
use std::collections::HashSet;

use bytecode::{
  SequenceHandle, SequenceInfo, InstrExpr, FunctionBytecode,
  Instr, Operator, LocalHandle, LocalInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use perm_alloc::{Ptr, perm_slice_from_vec, perm_slice, perm};

use symbols::Symbol;
use parse::SrcLocation;
use parse::{Expr, ExprShape, ExprTag, Val};

/// Compile expression to simple no-arg function
pub fn compile_expr_to_function(
  env : Env,
  resolver : &CellResolver,
  root : Expr) -> (Function, HashSet<CellUid>)
{
  let mut dependencies = HashSet::new();
  let f = compile_function(Builder::new(resolver, &mut dependencies, &env.c, env.st), root, &[], None);
  (f, dependencies)
}

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
struct Builder<'l> {
  bc : FunctionBytecode,
  
  /// local variables in scope
  scoped_vars : Vec<NamedVar>,
  
  /// the instruction sequence currently being built
  current_sequence : Option<SequenceHandle>,

  /// flags indicating which sequences have been finalised
  seq_completion : Vec<bool>,

  /// labels indicating the start and end of a labelled block expression
  label_stack : Vec<LabelledExpr>,

  resolver : &'l CellResolver<'l>,

  dependencies : &'l mut HashSet<CellUid>,

  c : &'l CoreTypes,

  st : SymbolTable,
}

impl <'l> Builder<'l> {
  fn new(
    resolver : &'l CellResolver<'l>,
    dependencies : &'l mut HashSet<CellUid>,
    c : &'l CoreTypes,
    st : SymbolTable,
  ) -> Self
  {
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
      resolver,
      dependencies,
      c, st,
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

fn find_label<'a>(b : &'a mut Builder, label : Symbol) -> &'a LabelledExpr {
  b.label_stack.iter().rev()
  .find(|l| l.name == label)
  .expect("label not found")
}

fn create_sequence(b : &mut Builder, name : &str) -> SequenceHandle {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(b.st, name);
  loop {
    let name_unique = b.bc.sequence_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(b.st, &format!("{}_{}", name, i));
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
  let f_addr = Box::into_raw(Box::new(f)) as i64;
  b.bc.instrs.push(Instr::Expr(function_var.id, InstrExpr::LiteralI64(f_addr)));
  function_var
}

fn compile_function_def(
  resolver : &CellResolver,
  dependencies : &mut HashSet<CellUid>,
  c : &CoreTypes,
  st : SymbolTable,
  args : &[Expr],
  return_tag : Option<Expr>,
  body : Expr,
) -> Function {
  let mut b = Builder::new(resolver, dependencies, c, st);
  b.bc.args = args.len();
  let mut arg_types = vec![];
  for a in args {
    if let Some(&[name, tag]) = a.as_syntax() {
      let t = const_expr_to_type(&mut b, tag);
      new_local_variable(&mut b, name.as_symbol(), t);
      arg_types.push(t);
    }
    else {
      panic!("expected function argument definition at ({})", a.loc())
    }
  }
  let expected_return = return_tag.map(|e| const_expr_to_type(&mut b, e));
  compile_function(b, body, &arg_types, expected_return)
}

fn compile_function(
  mut b : Builder, body : Expr, arg_types : &[TypeHandle],
  expected_return : Option<TypeHandle>) -> Function
{
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);

  let mut return_type = b.c.void_tag;
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

fn compile_if_else(b : &mut Builder, cond_expr : Expr, then_expr : Expr, else_expr : Expr) -> Option<Ref> {
  let mut result_var = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_var(b, cond_expr).id;
  assert_type(cond_expr.loc(), cond.t, b.c.bool_tag);
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
    panic!("expected value, found none at ({})", e.loc());
  }
}

fn compile_expr_to_var(b : &mut Builder, e : Expr) -> Var {
  let r = compile_expr_to_ref(b, e);
  r.to_var(b)
}

fn assert_type(loc : SrcLocation, t : TypeHandle, expected : TypeHandle) {
  if t != expected {
    panic!("expected {}, found {} at ({})", expected, t, loc)
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
  use ExprShape::*;
  use ExprTag::*;
  match e.shape() {
    // return
    List(Return, &[]) => {
      b.bc.instrs.push(Instr::Return(None));
      None
    }
    // return value
    List(Return, &[v]) => {
      let var = compile_expr_to_var(b, v);
      b.bc.instrs.push(Instr::Return(Some(var.id)));
      None
    }
    // break
    List(Break, &[]) => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // break to label
    List(Break, &[label]) => {
      let break_to = find_label(b, label.as_symbol()).exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // repeat
    List(Repeat, &[]) => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.bc.instrs.push(Instr::Jump(loop_back_to));
      None
    }
    // repeat to label
    List(Repeat, &[label]) => {
      let repeat_to = find_label(b, label.as_symbol()).entry_seq;
      b.bc.instrs.push(Instr::Jump(repeat_to));
      None
    }
    // boolean
    Literal(Val::Bool(v)) => {
      let e = InstrExpr::LiteralI64(if v { 1 } else { 0 });
      let v = push_expr(b, e, b.c.u64_tag);
      Some(compile_cast(b, v, b.c.bool_tag).to_ref())
    }
    // local reference
    Sym(sym) => {
      // Look for variable in local scope
      if let Some(v) = find_var_in_scope(b, sym) {
        Some(v.to_ref())
      }
      else {
        Some(compile_def_reference(b, e))
      }
    }
    List(Namespace, &[_namespace, _params]) => {
      Some(compile_def_reference(b, e))
    }
    List(ConstExpr, &[_]) => {
      let e = strip_const_wrapper(e);
      let cev = const_expr_value(b, e);
      let ie = InstrExpr::StaticValue(cev.t, cev.ptr);
      let v = push_expr(b, ie, types::pointer_type(cev.t));
      Some(pointer_to_locator(v, false))
    }
    // literal i64
    Literal(Val::I64(v)) => {
      let e = InstrExpr::LiteralI64(v);
      Some(push_expr(b, e, b.c.i64_tag).to_ref())
    }
    // literal string
    Literal(Val::String(s)) => {
      let t = types::pointer_type(b.c.string_tag);
      let e = InstrExpr::StaticValue(t, Ptr::to_ptr(s) as *const ());
      let v = push_expr(b, e, t);
      Some(pointer_to_locator(v, false))
    }
    // literal symbol
    List(LiteralSymbol, &[sym]) => {
      let s = sym.as_symbol();
      let e = InstrExpr::LiteralI64(s.as_i64());
      return Some(push_expr(b, e, b.c.symbol_tag).to_ref());
    }
    // literal void
    Literal(Val::Void) => {
      None
    }
    // array
    List(ArrayInit, elements) => {
      let e = compile_expr_to_var(b, elements[0]);
      let element_type = e.t;
      let mut element_values = vec![e.id];
      for e in &elements[1..] {
        let v = compile_expr_to_var(b, *e);
        if v.t != element_type {
          panic!("expected element of type {} and found type {}, at {}",
            element_type, v.t, e.loc());
        }
        element_values.push(v.id);
      }
      let t = types::array_type(element_type, elements.len() as i64);
      let e = InstrExpr::Array(perm_slice_from_vec(element_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // array length
    List(ArrayAsSlice, &[array]) => {
      let r = compile_expr_to_ref(b, array);
      let info = types::type_as_array(&r.t).expect("expected array");
      let array_ptr = r.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array_ptr, element_ptr_type);
      let len = compile_array_len(b, array);
      let e = InstrExpr::Init(perm_slice(&[ptr.id, len.id]));
      let t = types::slice_type(&b.c, b.st, info.inner);
      Some(push_expr(b, e, t).to_ref())
    }
    // array length
    List(ArrayLen, &[array]) => {
      Some(compile_array_len(b, array).to_ref())
    }
    // index
    List(Index, &[array , index]) => {
      let v = compile_expr_to_ref(b, array);
      let info = if let Some(info) = types::type_as_array(&v.t) {
        info
      }
      else {
        panic!("expected array at ({})", array.loc())
      };
      let array_ptr = v.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array_ptr, element_ptr_type);
      let offset = compile_expr_to_var(b, index).id;
      let e = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ptr.t);
      Some(pointer_to_locator(ptr, true))
    }
    // ptr index
    List(PtrIndex, &[ptr, index]) => {
      let ptr = compile_expr_to_var(b, ptr);
      let offset = compile_expr_to_var(b, index).id;
      let e = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ptr.t);
      Some(pointer_to_locator(ptr, true))
    }
    // zero init
    List(ZeroInit, &[t]) => {
      let t = const_expr_to_type(b, t);
      Some(push_expr(b, InstrExpr::ZeroInit, t).to_ref())      
    }
    // init
    List(StructInit, es) => {
      let type_expr = es[0];
      let field_vals = &es[1..];
      let t = const_expr_to_type(b, type_expr);
      let info = if let Some(i) = types::type_as_struct(&t) {
        i
      }
      else {
        panic!("expected struct, found {} at ({})",
          t, e.loc());
      };
      if field_vals.len() != info.field_types.len() {
        panic!("expected {} fields, found {}, at ({})",
          info.field_types.len(), field_vals.len(), e.loc());
      }
      let mut field_values = Vec::with_capacity(field_vals.len());
      for (i, f) in field_vals.iter().enumerate() {
        let v = compile_expr_to_var(b, *f);
        if info.field_types[i] != v.t  {
          panic!("expected arg of type {}, found type {}, at ({})",
            info.field_types[i], v.t, f.loc());
        }
        field_values.push(v.id);
      }
      let e = InstrExpr::Init(perm_slice_from_vec(field_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // field deref
    List(FieldIndex, &[structure, field_name]) => {
      let struct_ref = compile_and_fully_deref(b, structure);
      let struct_addr = struct_ref.get_address(b).id;
      let info =
        types::type_as_struct(&struct_ref.t)
        .unwrap_or_else(|| panic!("expected struct, found {} at ({})", struct_ref.t, e.loc()));
      let i = {
        let sym = field_name.as_symbol();
        info.field_names.as_slice().iter()
          .position(|n| *n == sym)
          .unwrap_or_else(||
            panic!("no such field '{}' at ({})", sym, field_name.loc())
          ) as u64
      };
      let field_type = info.field_types[i as usize];
      let e = InstrExpr::FieldIndex{ struct_addr, index: i };
      let v = push_expr(b, e, types::pointer_type(field_type));
      Some(pointer_to_locator(v, struct_ref.mutable))
    }
    // fun
    List(Fun, &[args, ret, body]) => {
      let return_tag = {
        if ret.tag == Omitted { None }
        else { Some(ret) }
      };
      let f = compile_function_def(
        b.resolver,
        b.dependencies,
        &b.c, b.st,
        args.children(),
        return_tag,
        body);
      let v = function_to_var(b, f);
      Some(v.to_ref())
    }
    // set var
    List(Set, &[dest, value]) => {
      let dest = compile_expr_to_ref(b, dest);
      let value = compile_expr_to_var(b, value);
      compile_assignment(b, dest, value);
      None
    }
    // let
    List(Let, &[var_name, value]) => {
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
    List(IfElse, &[cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond_var = compile_expr_to_var(b, cond).id;
      assert_type(cond.loc(), cond_var.t, b.c.bool_tag);
      b.bc.instrs.push(Instr::CJump{ cond: cond_var, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_expr(b, then_expr);
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    List(IfElse, &[cond, then_expr, else_expr]) => {
      compile_if_else(b, cond, then_expr, else_expr)
    }
    // label expression
    List(LabelledBlock, &[label, body]) => {
      let entry_seq = create_sequence(b, "block_entry");
      let exit_seq = create_sequence(b, "block_exit");
      b.bc.instrs.push(Instr::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledExpr{ name: label.as_symbol(), entry_seq, exit_seq }
      );
      let result = compile_expr(b, body);
      b.label_stack.pop();
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result.map(|x| x)
    }
    List(Do, exprs) => {
      let mut v = None;
      let num_locals = b.scoped_vars.len();
      for &c in exprs {
        // Ignore nested defs
        if c.tag != Def {
          v = compile_expr(b, c);
        }
      }
      b.scoped_vars.drain(num_locals..).for_each(|_| ());
      let result = v.map(|v| v.to_var(b)); // Can't pass ref out of block scope
      result.map(|x| x.to_ref())
    }
    // debug
    List(Debug, &[v]) => {
      let local = compile_expr_to_var(b, v);
      b.bc.instrs.push(Instr::Debug(v.loc(), local.id, local.t));
      None
    }
    // embed
    List(Embed, &[e]) => {
      let cev = const_expr_value(b, e);
      if cev.t != b.c.expr_tag {
        panic!("expected expression of type 'expr', found {}", cev.t);
      }
      let expr_value = unsafe { *(cev.ptr as *const Expr) };
      compile_expr(b, expr_value)
    }
    // typeof
    List(TypeOf, &[v]) => {
      // TODO: it's weird to codegen the expression when we only need its type
      let var = compile_expr_to_var(b, v);
      let e = InstrExpr::LiteralI64(Ptr::to_i64(var.t));
      return Some(push_expr(b, e, b.c.type_tag).to_ref());
    }
    // deref
    List(Deref, &[pointer]) => {
      let ptr = compile_expr_to_var(b, pointer);
      Some(pointer_to_locator(ptr, true))
    }
    // ref
    List(GetAddress, &[locator]) => {
      let local = compile_expr_to_ref(b, locator);
      return Some(local.get_address(b).to_ref())
    }
    // cast
    List(Cast, &[value, to_type]) => {
      let t = const_expr_to_type(b, to_type);
      let v = compile_expr_to_var(b, value);
      Some(compile_cast(b, v, t).to_ref())
    }
    // instrinsic op
    List(InstrinicOp, exprs) => {
      let op = exprs[0].as_operator_literal();
      let args = &exprs[1..];
      Some(compile_intrinic_op(b, e, op, args).to_ref())
    }
    // Call
    List(Call, exprs) => {
      let function_expr = exprs[0];
      let arg_exprs = &exprs[1..];
      Some(compile_function_call(b, e, function_expr, arg_exprs).to_ref())
    }
    // literal node
    List(Quote, &[e]) => {
      Some(compile_expr_value(b, e.deep_clone()).to_ref())
    }
    _ => {
      panic!("encountered invalid expression '{}' at ({})", e.loc().src_snippet(), e.loc())
    }
  }
}

fn compile_def_reference(b : &mut Builder, e : Expr) -> Ref {
  let uid = resolve_def_name(b, e);
  let cell = get_cell_value(b, uid);
  let e = InstrExpr::StaticValue(cell.t, cell.ptr);
  let pointer_type = types::pointer_type(cell.t);
  let v = push_expr(b, e, pointer_type);
  pointer_to_locator(v, true)
}

fn compile_array_len(b : &mut Builder, array : Expr) -> Var {
  let r = compile_expr_to_ref(b, array);
  let info = types::type_as_array(&r.t).expect("expected array");
  let e = InstrExpr::LiteralI64(info.length as i64);
  push_expr(b, e, b.c.i64_tag)
}

fn strip_const_wrapper(e : Expr) -> Expr {
  if let ExprShape::List(ExprTag::ConstExpr, &[ce]) = e.shape() {
    return ce;
  }
  panic!("expected const expression, found '{}' at ({})", e, e.loc())
}

fn const_expr_value(b : &mut Builder, e : Expr) -> CellValue {
  let uid = resolve_cell_id(b, CellId::ExprCell(e));
  let cell = get_cell_value(b, uid);
  if cell.e != e {
    panic!("const expressions '{}' and '{}' do not match", cell.e, e);
  }
  return cell;
}

fn const_expr_to_type(b: &mut Builder, e : Expr) -> TypeHandle {
  let e = strip_const_wrapper(e);
  let cev = const_expr_value(b, e);
  if cev.t != b.c.type_tag {
    panic!("expected expression of type 'type', found {}", cev.t);
  }
  unsafe { *(cev.ptr as *const TypeHandle) }
}

fn compile_function_call(b : &mut Builder, e : Expr, function_val : Expr, args : &[Expr]) -> Var {
  let mut arg_values = vec![];
  for &arg in args {
    arg_values.push(compile_expr_to_var(b, arg).id);
  }

  let f = compile_expr_to_var(b, function_val);
  let info = if let Some(i) = types::type_as_function(&f.t) {
    i
  }
  else {
    panic!("expected function, found {} at ({})",
      f.t, function_val.loc());
  };
  if args.len() != info.args.len() {
    panic!("expected {} args, found {}, at ({})",
      info.args.len(), args.len(), e.loc());
  }
  // Check the arg types
  for i in 0..args.len() {
    let arg = args[i];
    let arg_type = arg_values[i].t;
    let expected_type = info.args[i];
    if expected_type != arg_type  {
      panic!("expected arg of type {}, found type {}, at ({})", expected_type, arg_type, arg.loc());
    }
    if info.c_function && arg_type.size_of > 8 {
      panic!("types passed to a C function must be 64 bits wide or less; found type {} of width {} as ({})",
        arg_type, arg_type.size_of, arg.loc());
    }
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

fn binary_op_type(c : &CoreTypes, op : Operator, a : TypeHandle, b : TypeHandle) -> Option<TypeHandle> {
  use Operator::*;
  if a == b {
    match op {
      Add | Sub | Mul | Div | Rem | BitwiseAnd | BitwiseOr => {
        if types::is_number(a) {
          return Some(a);
        }
      }
      Eq | NEq => {
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
    if let Some(t) = binary_op_type(&b.c, op, v1.t, v2.t) {
      let e = InstrExpr::BinaryOp(op, v1.id, v2.id);
      return push_expr(b, e, t);
    }
    panic!("no binary op {} for types {} and {} at ({})",
      op, v1.t, v2.t, e.loc())
  }
  if let [v1] = args {
    let v1 = compile_expr_to_var(b, *v1);
    if let Some(t) = unary_op_type(&b.c, op, v1.t) {
      let e = InstrExpr::UnaryOp(op, v1.id);
      return push_expr(b, e, t);
    }
    panic!("no unary op {} for type {} at ({})",
      op, v1.t, e.loc())
  }
  panic!("incorrect number of args to operator {} at ({})", op, e.loc())
}

fn compile_expr_value(b : &mut Builder, expr : Expr) -> Var {
  let e = InstrExpr::LiteralI64(Ptr::to_ptr(expr) as i64);
  let expr_tag = b.c.expr_tag;
  push_expr(b, e, expr_tag)
}

fn resolve_def_name(b : &mut Builder, e : Expr) -> CellUid {
  if let Some(uid) = b.resolver.resolve_name(e) {
    b.dependencies.insert(uid);
    return uid;
  }
  panic!("def not found ({})", e.loc());
}

fn resolve_cell_id(b : &mut Builder, id : CellId) -> CellUid {
  if let Some(uid) = b.resolver.resolve_id(id) {
    if b.resolver.cell_status(uid) == CellStatus::Broken {
      panic!("dependency {} is broken", id);
    }
    b.dependencies.insert(uid);
    uid
  }
  else {
    panic!("dependency {} not found", id);
  }
}


fn get_cell_value(b : &mut Builder, uid : CellUid) -> CellValue {
  if let Some(v) = b.resolver.cell_value(uid) {
    return v;
  }
  panic!("no value found for cell {}", uid);
}

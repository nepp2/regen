
/// Compiles core language into bytecode

use crate::{bytecode, dependencies, env::{CellIdentifier, CellValue}, error::{Error, err, error}, hotload::CompileContext, parse, regen_alloc, symbols::{self, SymbolTable}, types};

use bytecode::{
  SequenceHandle, SequenceInfo, InstrExpr, FunctionBytecode,
  Instr, Operator, LocalHandle, LocalInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use regen_alloc::{Ptr, alloc_slice, alloc};

use symbols::Symbol;
use parse::{SrcLocation, templates::{self, ExprBuilder}};
use parse::{Expr, ExprShape, ExprTag, Val};

/// Compile expression to simple no-arg function
pub fn compile_expr_to_function(
  context : &CompileContext,
  root : Expr,
) -> Result<Function, Error>
{
  let f = compile_function(
    Builder::new(context, &context.env.c, context.env.st),
    root, &[], None)?;
  Ok(f)
}

struct LabelledExpr {
  name : Symbol,
  entry_seq: SequenceHandle,
  exit_seq: SequenceHandle,
}

#[derive(Clone)]
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

  context : &'l CompileContext<'l>,

  c : &'l CoreTypes,

  st : SymbolTable,
}

impl <'l> Builder<'l> {
  fn new(
    context : &'l CompileContext<'l>,
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
      context,
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
  loc : SrcLocation,
}

type ExprResult = Option<Ref>;

impl Ref {

  fn from_var(local : Var) -> Self {
    Ref{
      ref_type: RefType::Value(local.id),
      t: local.t,
      mutable: local.mutable,
      loc: local.loc,
    }
  }

  fn get_address(&self, b : &mut Builder) -> Var {
    let t = types::pointer_type(self.t);
    let id = match self.ref_type {
      RefType::Locator(id) => id,
      RefType::Value(id) => {
        let ie = InstrExpr::LocalAddr(id);
        let addr_id = new_local(b, None, t, self.mutable);
        b.bc.instrs.push(Instr::Expr(addr_id, ie));
        addr_id
      }
    };
    Var {
      id,
      t: t,
      mutable: self.mutable,
      loc: self.loc,
    }
  }
  
  fn to_var(&self, b : &mut Builder) -> Var {
    match self.ref_type {
      RefType::Locator(r) => {
        let ie = InstrExpr::Load(r);
        push_expr(b, self.loc, ie, self.t)
      }
      RefType::Value(r) => Var {
        id: r,
        t: self.t,
        mutable: self.mutable,
        loc: self.loc,
      },
    }
  }
}

#[derive(Copy, Clone)]
struct Var {
  id : LocalHandle,
  t : TypeHandle,
  mutable : bool,
  loc : SrcLocation,
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

fn new_local_variable<L>(b : &mut Builder, name : Symbol, t : TypeHandle, loc : L) -> Var
  where L : Into<SrcLocation>
{
  let id = new_local(b, Some(name), t, true);
  let v = Var { id, t: t, mutable: true, loc: loc.into() };
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
  let lh = alloc(info);
  b.bc.locals.push(lh);
  lh
}

fn new_var<L>(b : &mut Builder, t : TypeHandle, mutable : bool, loc : L) -> Var
  where L : Into<SrcLocation>
{
  let id = new_local(b, None, t, mutable);
  Var { id, t: t, mutable, loc: loc.into() }
}

fn push_expr<L>(b : &mut Builder, loc : L, ie : InstrExpr, t : TypeHandle) -> Var
  where L : Into<SrcLocation>
{
  let v = new_var(b, t, false, loc.into());
  b.bc.instrs.push(Instr::Expr(v.id, ie));
  return v;
}

fn pointer_to_locator(v : Var, mutable : bool) -> Result<Ref, Error> {
  let inner = types::deref_pointer_type(v.t)
    .ok_or_else(|| error(v.loc, "expected pointer type"))?;
  let r = Ref {
    ref_type: RefType::Locator(v.id),
    t: inner,
    mutable,
    loc: v.loc,
  };
  Ok(r)
}

fn find_label<'a>(b : &'a mut Builder, label : Expr) -> Result<&'a LabelledExpr, Error> {
  let sym = label.as_symbol();
  b.label_stack.iter().rev()
  .find(|l| l.name == sym)
  .ok_or_else(|| error(label, format!("label '{}' not found", sym)))
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
  let seq = alloc(SequenceInfo {
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

fn compile_function_def(
  context : &CompileContext,
  c : &CoreTypes,
  st : SymbolTable,
  args : &[Expr],
  return_tag : Option<Expr>,
  body : Expr,
) -> Result<Function, Error>
{
  let mut b = Builder::new(context, c, st);
  b.bc.args = args.len();
  let mut arg_types = vec![];
  for &a in args {
    if let Some(&[name, tag]) = a.as_syntax() {
      let t = const_expr_to_type(&mut b, tag)?;
      new_local_variable(&mut b, name.as_symbol(), t, a);
      arg_types.push(t);
    }
    else {
      return err(a, "expected function argument definition")
    }
  }
  let expected_return =
    if let Some(e) = return_tag { 
      Some(const_expr_to_type(&mut b, e)?)
    }
    else { None };
  compile_function(b, body, &arg_types, expected_return)
}

fn compile_function(
  mut b : Builder, body : Expr, arg_types : &[TypeHandle],
  expected_return : Option<TypeHandle>
) -> Result<Function, Error>
{
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);

  let mut return_type = b.c.void_tag;
  let result = compile_expr(&mut b, body)?;
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
      return err(body, format!("expected return type {}, found {}", et, return_type));
    }
  }
  let t = types::function_type(&arg_types, return_type);
  let f = complete_function(b);
  // for n in body { println!("{}", n) }
  // println!("{}", f);
  Ok(Function { bc: f, t })
}

fn compile_if_else(
  b : &mut Builder, cond_expr : Expr,
  then_expr : Expr, else_expr : Expr
) -> Result<ExprResult, Error>
{
  let mut result_var = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_var(b, cond_expr)?.id;
  assert_type(cond_expr, cond.t, b.c.bool_tag)?;
  b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result = compile_expr(b, then_expr)?;
  if let Some(v) = then_result {
    let nv = new_var(b, v.t, true, then_expr).to_ref();
    result_var = Some(nv);
    let v = v.to_var(b);
    compile_assignment(b, then_expr, nv, v)?;
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result = compile_expr(b, else_expr)?;
  if let Some(l) = result_var {
    let v = else_result
      .ok_or_else(||
        error(else_expr, format!("expected else value of type '{}', found void", l.t))
      )?
      .to_var(b);
    compile_assignment(b, else_expr, l, v)?;
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  Ok(result_var)
}

fn compile_assignment<L : Into<SrcLocation>>(b : &mut Builder, loc : L, dest : Ref, value : Var)
  -> Result<(), Error>
{
  if dest.t.size_of != value.t.size_of {
    return err(loc, "types don't match");
  }
  if !dest.mutable {
    return err(loc, "can't assign to this value");
  }
  let pointer = dest.get_address(b).id;
  b.bc.instrs.push(Instr::Store {
    pointer,
    value: value.id,
  });
  Ok(())
}

/// dereferences any pointer types encountered
fn compile_and_fully_deref(b : &mut Builder, e : Expr) -> Result<Ref, Error> {
  let mut r = compile_expr_to_ref(b, e)?;
  while types::deref_pointer_type(r.t).is_some() {
    let v = r.to_var(b);
    r = pointer_to_locator(v, r.mutable)?;
  }
  Ok(r)
}

fn compile_expr_to_ref(b : &mut Builder, e : Expr) -> Result<Ref, Error> {
  if let Some(r) = compile_expr(b, e)? {
    Ok(r)
  }
  else {
    err(e, "expected value, found none")
  }
}

fn compile_expr_to_var(b : &mut Builder, e : Expr) -> Result<Var, Error> {
  let r = compile_expr_to_ref(b, e)?;
  Ok(r.to_var(b))
}

fn assert_type<L : Into<SrcLocation>>(loc : L, t : TypeHandle, expected : TypeHandle)
  -> Result<(), Error>
{
  if t != expected {
    return err(loc.into(), format!("expected {}, found {}", expected, t));
  }
  Ok(())
}

fn compile_cast<L>(b : &mut Builder, loc : L, v : Var, t : TypeHandle)
  -> Result<Var, Error>
    where L : Into<SrcLocation>
{
  fn ptr_or_prim(t : TypeHandle) -> bool {
    let t = types::strip_alias(t);
    t.kind == Kind::Pointer || t.kind == Kind::Primitive
  }

  if t.size_of != v.t.size_of {
    if !(ptr_or_prim(t) && ptr_or_prim(v.t)) {
      return err(loc, format!("cannot cast from {} to {}", v.t, t));
    }
  }
  Ok(push_expr(b, v.loc, InstrExpr::Cast(v.id), t))
}

/// Returns true for tags that the code generation ignores
/// These are used by other compile stages
fn is_ignored_tag(t : ExprTag) -> bool {
  use ExprTag::*;
  match t {
    Def | Reactive | Cells => true,
    _ => false,
  }
}

fn compile_expr(b : &mut Builder, e : Expr) -> Result<ExprResult, Error> {
  if is_ignored_tag(e.tag) {
    return Ok(None);
  }
  use ExprShape::*;
  use ExprTag::*;
  match e.shape() {
    // return
    List(Return, &[]) => {
      b.bc.instrs.push(Instr::Return(None));
    }
    // return value
    List(Return, &[v]) => {
      let var = compile_expr_to_var(b, v)?;
      b.bc.instrs.push(Instr::Return(Some(var.id)));
    }
    // break
    List(Break, &[]) => {
      let break_to =
        b.label_stack.last()
        .ok_or_else(|| error(e, "can't break in this scope"))?
        .exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
    }
    // break to label
    List(Break, &[label]) => {
      let break_to = find_label(b, label)?.exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
    }
    // repeat
    List(Repeat, &[]) => {
      let loop_back_to =
        b.label_stack.last()
        .ok_or_else(|| error(e, "can't repeat in this scope"))?
        .entry_seq;
      b.bc.instrs.push(Instr::Jump(loop_back_to));
    }
    // repeat to label
    List(Repeat, &[label]) => {
      let repeat_to = find_label(b, label)?.entry_seq;
      b.bc.instrs.push(Instr::Jump(repeat_to));
    }
    // boolean
    Literal(Val::Bool(v)) => {
      let lit_expr = InstrExpr::LiteralI64(if v { 1 } else { 0 });
      let v = push_expr(b, e, lit_expr, b.c.u64_tag);
      return Ok(Some(compile_cast(b, e, v, b.c.bool_tag)?.to_ref()));
    }
    // local or global reference
    Sym(sym) => {
      // Look for variable in local scope
      if let Some(v) = find_var_in_scope(b, sym) {
        return Ok(Some(v.to_ref()));
      }
      else {
        return Ok(Some(compile_def_reference(b, e)?));
      }
    }
    List(Namespace, &[_namespace, _params]) => {
      return Ok(Some(compile_def_reference(b, e)?));
    }
    List(ConstExpr, &[_]) => {
      let e = strip_const_wrapper(e)?;
      let cev = const_expr_value(b, e)?;
      let ie = InstrExpr::StaticValue(cev.t, cev.ptr);
      let v = push_expr(b, e, ie, types::pointer_type(cev.t));
      return Ok(Some(pointer_to_locator(v, false)?));
    }
    // literal i64
    Literal(Val::I64(v)) => {
      let ie = InstrExpr::LiteralI64(v);
      return Ok(Some(push_expr(b, e, ie, b.c.i64_tag).to_ref()));
    }
    // literal string
    Literal(Val::String(s)) => {
      let t = types::pointer_type(b.c.string_tag);
      let ie = InstrExpr::StaticValue(t, Ptr::to_ptr(s) as *const ());
      let v = push_expr(b, e, ie, t);
      return Ok(Some(pointer_to_locator(v, false)?));
    }
    // literal symbol
    List(LiteralSymbol, &[sym]) => {
      let s = sym.as_symbol();
      let ie = InstrExpr::LiteralI64(s.as_i64());
      return Ok(Some(push_expr(b, e, ie, b.c.symbol_tag).to_ref()));
    }
    // literal void
    Literal(Val::Void) => {
      ()
    }
    // array
    List(ArrayInit, &[type_expr, elements]) => {
      let mut element_values = vec![];
      let mut el_iter = elements.children().iter().cloned();
      let element_type = {
        if type_expr.tag == Omitted {
          let first_el = el_iter.next().unwrap();
          let v = compile_expr_to_var(b, first_el)?;
          element_values.push(v.id);
          v.t
        }
        else {
          const_expr_to_type(b, type_expr)?
        }
      };
      for el in el_iter {
        let v = compile_expr_to_var(b, el)?;
        if v.t != element_type {
          return err(el,
            format!("expected element of type {} and found type {}",
              element_type, v.t));
        }
        element_values.push(v.id);
      }
      let t = types::array_type(element_type, element_values.len() as i64);
      let ie = InstrExpr::Array(alloc_slice(element_values));
      return Ok(Some(push_expr(b, e, ie, t).to_ref()))
    }
    // array length
    List(ArrayAsSlice, &[array]) => {
      let r = compile_expr_to_ref(b, array)?;
      let info =
        types::type_as_array(r.t)
        .ok_or_else(|| error(array, "expected array"))?;
      let array_ptr = r.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array, array_ptr, element_ptr_type)?;
      let len = compile_array_len(b, array)?;
      let ie = InstrExpr::Init(alloc_slice([ptr.id, len.id]));
      let t = types::slice_type(&b.c, b.st, info.inner);
      return Ok(Some(push_expr(b, e, ie, t).to_ref()))
    }
    // array length
    List(ArrayLen, &[array]) => {
      return Ok(Some(compile_array_len(b, array)?.to_ref()))
    }
    // index
    List(Index, &[array , index]) => {
      let v = compile_expr_to_ref(b, array)?;
      let info = if let Some(info) = types::type_as_array(v.t) {
        info
      }
      else {
        return err(array, "expected array");
      };
      let array_ptr = v.get_address(b);
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_cast(b, array, array_ptr, element_ptr_type)?;
      let offset = compile_expr_to_var(b, index)?.id;
      let ie = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ie, ptr.t);
      return Ok(Some(pointer_to_locator(ptr, true)?));
    }
    // ptr index
    List(PtrIndex, &[ptr, index]) => {
      let ptr = compile_expr_to_var(b, ptr)?;
      let offset = compile_expr_to_var(b, index)?.id;
      let ie = InstrExpr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ie, ptr.t);
      return Ok(Some(pointer_to_locator(ptr, true)?));
    }
    // zero init
    List(ZeroInit, &[t]) => {
      let t = const_expr_to_type(b, t)?;
      return Ok(Some(push_expr(b, e, InstrExpr::ZeroInit, t).to_ref()));
    }
    // init
    List(StructInit, es) => {
      let type_expr = es[0];
      let field_vals = &es[1..];
      let t = const_expr_to_type(b, type_expr)?;
      let info = if let Some(i) = types::type_as_struct(t) {
        i
      }
      else {
        return err(e, format!("expected struct, found {}", t));
      };
      if field_vals.len() != info.field_types.len() {
        return err(e,
          format!("expected {} fields, found {}",
            info.field_types.len(), field_vals.len()));
      }
      let mut field_values = Vec::with_capacity(field_vals.len());
      for (i, f) in field_vals.iter().enumerate() {
        let v = compile_expr_to_var(b, *f)?;
        if info.field_types[i] != v.t  {
          return err(f,
            format!("expected arg of type {}, found type {}",
              info.field_types[i], v.t));
        }
        field_values.push(v.id);
      }
      let ie = InstrExpr::Init(alloc_slice(field_values));
      return Ok(Some(push_expr(b, e, ie, t).to_ref()));
    }
    // field deref
    List(FieldIndex, &[structure, field_name]) => {
      let struct_ref = compile_and_fully_deref(b, structure)?;
      let struct_addr = struct_ref.get_address(b).id;
      let info =
        types::type_as_struct(struct_ref.t)
        .ok_or_else(||
          error(e, format!("expected struct, found {}", struct_ref.t))
        )?;
      let i = {
        let sym = field_name.as_symbol();
        info.field_names.as_slice().iter()
          .position(|n| *n == sym)
          .ok_or_else(||
            error(field_name, format!("no such field '{}'", sym))
          )? as u64
      };
      let field_type = info.field_types[i as usize];
      let ie = InstrExpr::FieldIndex{ struct_addr, index: i };
      let v = push_expr(b, e, ie, types::pointer_type(field_type));
      return Ok(Some(pointer_to_locator(v, struct_ref.mutable)?));
    }
    // fun
    List(Fun, &[args, ret, body]) => {
      let return_tag = {
        if ret.tag == Omitted { None }
        else { Some(ret) }
      };
      let f = compile_function_def(
        b.context,
        &b.c, b.st,
        args.children(),
        return_tag,
        body)?;
      let v = {
        let function_var = new_var(b, f.t, false, e.loc());
        let f_addr = Box::into_raw(Box::new(f)) as i64;
        b.bc.instrs.push(Instr::Expr(function_var.id, InstrExpr::LiteralI64(f_addr)));
        function_var
      };
      return Ok(Some(v.to_ref()));
    }
    // set var
    List(Set, &[dest_expr, value_expr]) => {
      let dest = compile_expr_to_ref(b, dest_expr)?;
      let value = compile_expr_to_var(b, value_expr)?;
      compile_assignment(b, value_expr, dest, value)?;
    }
    // let
    List(Let, &[var_name, value_expr]) => {
      // TODO: this generates redundant bytecode. If the value is a
      // locator, it should just generate a load, rather than a load and a store.
      let name = var_name.as_symbol();
      // evaluate the expression
      let value = compile_expr_to_var(b, value_expr)?;
      let local_var = new_local_variable(b, name, value.t, value_expr);
      compile_assignment(b, value_expr, local_var.to_ref(), value)?;
    }
    // if then
    List(IfElse, &[cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond_var = compile_expr_to_var(b, cond)?.id;
      assert_type(cond, cond_var.t, b.c.bool_tag)?;
      b.bc.instrs.push(Instr::CJump{ cond: cond_var, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_expr(b, then_expr)?;
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
    }
    // if then else
    List(IfElse, &[cond, then_expr, else_expr]) => {
      return compile_if_else(b, cond, then_expr, else_expr);
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
      let result = compile_expr(b, body)?;
      b.label_stack.pop();
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      return Ok(result);
    }
    List(Do, exprs) => {
      let mut v = None;
      let num_locals = b.scoped_vars.len();
      for &c in exprs {
        // Ignore nested defs
        if !is_ignored_tag(c.tag) {
          v = compile_expr(b, c)?;
        }
      }
      b.scoped_vars.drain(num_locals..).for_each(|_| ());
      let result = v.map(|v| v.to_var(b)); // Can't pass ref out of block scope
      return Ok(result.map(|x| x.to_ref()));
    }
    // debug
    List(Debug, &[v]) => {
      let local = compile_expr_to_var(b, v)?;
      b.bc.instrs.push(Instr::Debug(v.loc(), local.id, local.t));
    }
    // embed
    List(Embed, &[e]) => {
      let uid = CellIdentifier::EmbedCell(e);
      let cv = get_cell_value(b, e, uid)?;
      let ie = InstrExpr::StaticValue(cv.t, cv.ptr);
      let v = push_expr(b, e, ie, types::pointer_type(cv.t));
      return Ok(Some(pointer_to_locator(v, false)?));
    }
    // typeof
    List(TypeOf, &[v]) => {
      // TODO: it's weird to codegen the expression when we only need its type
      let var = compile_expr_to_var(b, v)?;
      return Ok(Some(compile_type_literal(b, var.t, v).to_ref()));
    }
    // deref
    List(Deref, &[pointer]) => {
      let ptr = compile_expr_to_var(b, pointer)?;
      return Ok(Some(pointer_to_locator(ptr, true)?));
    }
    // ref
    List(GetAddress, &[locator]) => {
      let local = compile_expr_to_ref(b, locator)?;
      return Ok(Some(local.get_address(b).to_ref()));
    }
    // cast
    List(Cast, &[value, to_type]) => {
      let t = const_expr_to_type(b, to_type)?;
      let v = compile_expr_to_var(b, value)?;
      return Ok(Some(compile_cast(b, e, v, t)?.to_ref()));
    }
    // observe
    List(Observe, &[e]) => {
      let id = expr_to_id(e)?;      
      let cell = get_cell_value(b, e, id)?;
      let signal_type = types::poly_type(b.c.signal_tag, cell.t);
      let uid_ptr = Ptr::to_ptr(alloc(id)) as *const ();
      let ie = InstrExpr::StaticValue(signal_type, uid_ptr);
      let v = push_expr(b, e, ie, signal_type);
      return Ok(Some(v.to_ref()));
    }
    // container
    List(Container, &[signal_expr, value_expr, function_expr]) => {
      let c = compile_container(b, e, signal_expr, value_expr, function_expr)?;
      return Ok(Some(c.to_ref()));
    }
    // stream
    List(Stream, &[signal_expr, function_expr]) => {
      let s = compile_stream(b, e, signal_expr, function_expr)?;
      return Ok(Some(s.to_ref()));
    }
    // instrinsic op
    List(InstrinicOp, exprs) => {
      let op = exprs[0].as_operator_literal();
      let args = &exprs[1..];
      return Ok(Some(compile_intrinic_op(b, e, op, args)?.to_ref()));
    }
    // call
    List(Call, exprs) => {
      let function_expr = exprs[0];
      let arg_exprs = &exprs[1..];
      let mut arg_values = vec![];
      for &arg in arg_exprs {
        arg_values.push(compile_expr_to_var(b, arg)?.id);
      }
      let v = compile_function_call(b, e, function_expr, arg_exprs, arg_values)?.to_ref();
      if v.t == b.c.void_tag {
        return Ok(None);
      }
      return Ok(Some(v));
    }
    // literal node
    List(Quote, &[e]) => {
      return Ok(Some(compile_expr_value(b, e.deep_clone()).to_ref()));
    }
    _ => {
      return err(e,
        format!("encountered invalid expression '{}'", e));
    }
  }
  Ok(None)
}

fn expr_to_id(e : Expr) -> Result<CellIdentifier, Error> {
  dependencies::expr_to_id(e).ok_or_else(|| error(e, "malformed identifier"))
}

fn compile_type_literal<L>(b : &mut Builder, t : TypeHandle, loc : L) -> Var
  where L : Into<SrcLocation>
{
  let ie = InstrExpr::LiteralI64(Ptr::to_i64(t.0));
  return push_expr(b, loc, ie, b.c.type_tag);
}

fn compile_def_reference(b : &mut Builder, e : Expr) -> Result<Ref, Error> {
  let uid = expr_to_id(e)?;
  let cell = get_cell_value(b, e, uid)?;
  let ie = InstrExpr::StaticValue(cell.t, cell.ptr);
  let pointer_type = types::pointer_type(cell.t);
  let v = push_expr(b, e, ie, pointer_type);
  Ok(pointer_to_locator(v, true)?)
}

fn compile_array_len(b : &mut Builder, array : Expr) -> Result<Var, Error> {
  let r = compile_expr_to_ref(b, array)?;
  let info =
    types::type_as_array(r.t)
    .ok_or_else(|| error(array, "expected array"))?;
  let ie = InstrExpr::LiteralI64(info.length as i64);
  Ok(push_expr(b, array, ie, b.c.i64_tag))
}

fn strip_const_wrapper(e : Expr) -> Result<Expr, Error> {
  if let ExprShape::List(ExprTag::ConstExpr, &[ce]) = e.shape() {
    return Ok(ce);
  }
  return err(e, format!("expected const expression, found '{}'", e));
}

fn const_expr_value(b : &mut Builder, e : Expr) -> Result<CellValue, Error> {
  let uid = CellIdentifier::expr(e);
  let cell = get_cell_value(b, e, uid)?;
  return Ok(cell);
}

fn const_expr_to_type(b: &mut Builder, e : Expr) -> Result<TypeHandle, Error> {
  let e = strip_const_wrapper(e)?;
  let cev = const_expr_value(b, e)?;
  if cev.t != b.c.type_tag {
    return err(e,
      format!("expected expression of type 'type', found {}", cev.t));
  }
  Ok(unsafe { *(cev.ptr as *const TypeHandle) })
}

fn compile_function_call(
  b : &mut Builder,
  e : Expr,
  function_val : Expr,
  args : &[Expr],
  arg_values : Vec<LocalHandle>
) -> Result<Var, Error>
{
  let f = compile_expr_to_var(b, function_val)?;
  let info = if let Some(i) = types::type_as_function(f.t) {
    i
  }
  else {
    return err(function_val,
      format!("expected function, found {}", f.t));
  };
  if args.len() != info.args.len() {
    return err(e,
      format!("expected {} args, found {}",
        info.args.len(), args.len()));
  }
  // Check the arg types
  for i in 0..args.len() {
    let arg = args[i];
    let arg_type = arg_values[i].t;
    let expected_type = info.args[i];
    if expected_type != arg_type  {
      return err(arg,
        format!("expected arg of type {}, found type {}",
          expected_type, arg_type));
    }
    if info.c_function && arg_type.size_of > 8 {
      return err(arg,
        format!("types passed to a C function must be 64 bits wide or less; found type {} of width {}",
          arg_type, arg_type.size_of));
    }
  }
  let ie = {
    if info.c_function {
      InstrExpr::InvokeC(f.id, alloc_slice(arg_values))
    }
    else {
      InstrExpr::Invoke(f.id, alloc_slice(arg_values))
    }
  };
  return Ok(push_expr(b, e, ie, info.returns));
}

fn compile_container(
  b : &mut Builder,
  e : Expr,
  signal_expr : Expr,
  value_expr : Expr,
  function_expr : Expr,
) -> Result<Var, Error>
{
  let eb = ExprBuilder::new(e.loc(), b.st);
  let container_function = templates::create_container_ref(&eb);
  let signal = compile_expr_to_var(b, signal_expr)?;
  let value = compile_expr_to_var(b, value_expr)?;
  let function = compile_expr_to_var(b, function_expr)?;
  // Check types
  let fun =
    types::type_as_function(function.t)
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let state_type =
    types::deref_pointer_type(fun.args[0])
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let input_type =
    types::deref_pointer_type(fun.args[1])
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let signal_param =
    types::type_as_poly(signal.t)
    .ok_or_else(|| error(signal_expr, "invalid signal type"))?
    .param;
  if state_type != value.t {
    return err(e,
      format!("conflicting container state types {} and {}",
      state_type, value.t));
  }
  if signal_param != input_type {
    return err(e,
      format!("conflicting container input types {} and {}",
      signal_param, input_type));
  }
  // Cast args
  let void_ptr_tag = types::pointer_type(b.c.void_tag);
  let signal_arg = compile_cast(b, e, signal, b.c.signal_tag)?;
  let type_arg = compile_type_literal(b, value.t, value.loc);
  let value_arg = {
    let v = value.to_ref().get_address(b);
    compile_cast(b, e, v, void_ptr_tag)?
  };
  let function_arg = compile_cast(b, e, function, void_ptr_tag)?;
  let ret = compile_function_call(
    b, e,
    container_function,
    &[signal_expr, value_expr, value_expr, function_expr],
    vec![signal_arg.id, type_arg.id, value_arg.id, function_arg.id],
  )?;
  let constructor_type = types::poly_type(b.c.reactive_constructor_tag, value.t);
  compile_cast(b, e, ret, constructor_type)
}

fn compile_stream(
  b : &mut Builder,
  e : Expr,
  signal_expr : Expr,
  function_expr : Expr,
) -> Result<Var, Error>
{
  let eb = ExprBuilder::new(e.loc(), b.st);
  let stream_function = templates::create_stream_ref(&eb);
  let signal = compile_expr_to_var(b, signal_expr)?;
  let function = compile_expr_to_var(b, function_expr)?;
  // Check types
  let fun =
    types::type_as_function(function.t)
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let state_type =
    types::deref_pointer_type(fun.args[0])
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let input_type =
    types::deref_pointer_type(fun.args[1])
    .ok_or_else(|| error(function_expr, "invalid function type"))?;
  let signal_param =
    types::type_as_poly(signal.t)
    .ok_or_else(|| error(signal_expr, "invalid signal type"))?
    .param;
  if signal_param != input_type {
    return err(e,
      format!("conflicting container input types {} and {}",
        signal_param, input_type));
  }
  // Cast args
  let void_ptr_tag = types::pointer_type(b.c.void_tag);
  let signal_arg = compile_cast(b, e, signal, b.c.signal_tag)?;
  let type_arg = compile_type_literal(b, state_type, function_expr);
  let function_arg = compile_cast(b, e, function, void_ptr_tag)?;
  let ret = compile_function_call(
    b, e,
    stream_function,
    &[signal_expr, function_expr, function_expr],
    vec![signal_arg.id, type_arg.id, function_arg.id],
  )?;
  let constructor_type = types::poly_type(b.c.reactive_constructor_tag, state_type);
  compile_cast(b, e, ret, constructor_type)
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
      And | Or => {
        if types::is_bool(a) {
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

fn compile_intrinic_op(b : &mut Builder, e : Expr, op : Operator, args : &[Expr]
) -> Result<Var, Error>
{
  if let [v1, v2] = args {
    let v1 = compile_expr_to_var(b, *v1)?;
    let v2 = compile_expr_to_var(b, *v2)?;
    if let Some(t) = binary_op_type(&b.c, op, v1.t, v2.t) {
      let ie = InstrExpr::BinaryOp(op, v1.id, v2.id);
      return Ok(push_expr(b, e, ie, t));
    }
    return err(e,
      format!("no binary op {} for types {} and {} at",
        op, v1.t, v2.t));
  }
  if let [v1] = args {
    let v1 = compile_expr_to_var(b, *v1)?;
    if let Some(t) = unary_op_type(&b.c, op, v1.t) {
      let ie = InstrExpr::UnaryOp(op, v1.id);
      return Ok(push_expr(b, e, ie, t));
    }
    return err(e,
      format!("no unary op {} for type {}", op, v1.t));
  }
  return err(e,
    format!("incorrect number of args to operator {}", op));
}

fn compile_expr_value(b : &mut Builder, expr : Expr) -> Var {
  let ie = InstrExpr::LiteralI64(Ptr::to_ptr(expr) as i64);
  let expr_tag = b.c.expr_tag;
  push_expr(b, expr, ie, expr_tag)
}

fn get_cell_value(b : &mut Builder, e : Expr, id : CellIdentifier) -> Result<CellValue, Error> {
  if let Some(v) = b.context.cell_value(id) {
    if v.initialised {
      return Ok(*v);
    }
    else {
      return err(e, format!("cell {} not yet initialised", id));
    }
  }
  err(e, format!("no value found for {}", id))
}

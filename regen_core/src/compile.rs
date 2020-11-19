/// Compiles core language into bytecode

use crate::{
  bytecode, parse, symbols, env, perm_alloc,
  types, interpret, node_macros,
};

use bytecode::{
  SequenceId, SequenceInfo, Expr, FunctionBytecode,
  Instr, Operator, LocalId, LocalInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use perm_alloc::{Perm, perm_slice_from_vec};

use symbols::{to_symbol, Symbol};
use env::Env;
use parse::{
  Node, NodeInfo, NodeContent, NodeLiteral,
  node_shape, NodeShape,
};
use NodeShape::*;
use node_macros::{NodeBuilder, def_macro, template_macro};

struct LabelledExpr {
  name : Symbol,
  entry_seq: SequenceId,
  exit_seq: SequenceId,
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
  current_sequence : Option<SequenceId>,

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
  Locator(LocalId),

  /// A value register can be read from directly, but can't be written to.
  Value(LocalId),
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
        let e = Expr::LocalAddr(id);
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
        let e = Expr::Load(r);
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
  id : LocalId,
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

fn new_local(b : &mut Builder, name : Option<Symbol>, t : TypeHandle, mutable : bool) -> LocalId {
  let id = LocalId { id: b.bc.locals.len() };
  let info = LocalInfo {
    id,
    name,
    byte_offset: 0,
    t,
    mutable,
  };
  b.bc.locals.push(info);
  id
}

fn new_var(b : &mut Builder, t : TypeHandle, mutable : bool) -> Var {
  let id = new_local(b, None, t, mutable);
  Var { id, t: t, mutable }
}

fn push_expr(b : &mut Builder, e : Expr, t : TypeHandle) -> Var {
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

fn create_sequence(b : &mut Builder, name : &str) -> SequenceId {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(b.env.st, name);
  loop {
    let name_unique = b.bc.sequence_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(b.env.st, &format!("{}_{}", name, i));
  }
  let seq_id = SequenceId(b.bc.sequence_info.len());
  b.bc.sequence_info.push(SequenceInfo {
    name: name_candidate,
    start_instruction: 0, num_instructions: 0,
  });
  b.seq_completion.push(false);
  seq_id
}

fn set_current_sequence(b : &mut Builder, sequence : SequenceId) {
  // complete the current sequence (if there is one)
  complete_sequence(b);
  // check that the sequence isn't done yet
  if b.seq_completion[sequence.0] {
    panic!("this sequence has already been completed");
  }
  b.current_sequence = Some(sequence);
  b.bc.sequence_info[sequence.0].start_instruction = b.bc.instrs.len();
}

fn complete_sequence(b : &mut Builder) {
  if let Some(i) = b.current_sequence {
    let seq = &mut b.bc.sequence_info[i.0];
    seq.num_instructions = b.bc.instrs.len() - seq.start_instruction;
    b.seq_completion[i.0] = true;
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

fn compile_macro_def(env: Env, arg : Node, body : Node) -> Function {
  let mut b = Builder::new(env);
  b.bc.args = 1;
  let name = arg.as_symbol();
  let arg_type = env.c.node_slice_tag;
  new_local_variable(&mut b, name, arg_type);
  compile_function(b, body, &[arg_type], Some(env.c.node_tag))
}

fn function_to_var(b : &mut Builder, f : Function) -> Var {
  let function_var = new_var(b, f.t, false);
  let f_addr = Box::into_raw(Box::new(f)) as u64;
  b.bc.instrs.push(Instr::Expr(function_var.id, Expr::LiteralU64(f_addr)));
  function_var
}

fn compile_function_def(env: Env, args : &[Node], return_tag : Option<Node>, body : Node) -> Function {
  let mut b = Builder::new(env);
  b.bc.args = args.len();
  let mut arg_types = vec![];
  for &arg in args {
    let (name, t) = {
      if let [name, type_tag] = arg.children() {
        let t = node_to_type(&b, *type_tag);
        (name.as_symbol(), t)
      }
      else {
        panic!("expected typed argument")
      }
    };
    new_local_variable(&mut b, name, t);
    arg_types.push(t);
  }
  let expected_return = return_tag.map(|n| node_to_type(&mut b, n));
  compile_function(b, body, &arg_types, expected_return)
}

fn compile_function(
  mut b : Builder, body : Node, arg_types : &[TypeHandle],
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
pub fn compile_expr_to_function(env: Env, root : Node) -> Function {
  compile_function(Builder::new(env), root, &[], None)
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> Option<Ref> {
  let mut result_var = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_var(b, cond).id;
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
fn compile_and_fully_deref(b : &mut Builder, node : Node) -> Ref {
  let mut r = compile_expr_to_ref(b, node);
  while r.t.kind == Kind::Pointer {
    let v = r.to_var(b);
    r = pointer_to_locator(v, r.mutable);
  }
  r
}

fn compile_expr_to_ref(b : &mut Builder, node : Node) -> Ref {
  if let Some(r) = compile_expr(b, node) {
    r
  }
  else {
    panic!("expected value, found none, at node {} ({})", node, node.loc);
  }
}

fn compile_expr_to_var(b : &mut Builder, node : Node) -> Var {
  let r = compile_expr_to_ref(b, node);
  r.to_var(b)
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
  push_expr(b, Expr::Cast(v.id), t)
}

fn compile_expr(b : &mut Builder, node : Node) -> Option<Ref> {
  match node_shape(&node) {
    // return
    Atom("return") => {
      b.bc.instrs.push(Instr::Return(None));
      None
    }
    // break
    Atom("break") => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // repeat
    Atom("repeat") => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.bc.instrs.push(Instr::Jump(loop_back_to));
      None
    }
    // true
    Atom("true") => {
      let e = Expr::LiteralU64(1);
      Some(push_expr(b, e, b.env.c.u64_tag).to_ref())
    }
    // false
    Atom("false") => {
      let e = Expr::LiteralU64(0);
      Some(push_expr(b, e, b.env.c.u64_tag).to_ref())
    }
    // symbol reference (local var or global def)
    Atom(_) => {
      Some(compile_symbol_reference(b, node))
    }
    // literal
    Literal(l) => {
      Some(compile_literal(b, l))
    }
    // break to label
    Command("break", [label]) => {
      let break_to = find_label(b, label.as_symbol()).exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // repeat to label
    Command("repeat", [label]) => {
      let repeat_to = find_label(b, label.as_symbol()).entry_seq;
      b.bc.instrs.push(Instr::Jump(repeat_to));
      None
    }
    // array
    Command("array", elements) => {
      let e = compile_expr_to_var(b, elements[0]);
      let element_type = e.t;
      let mut element_values = vec![e.id];
      for e in &elements[1..] {
        let v = compile_expr_to_var(b, *e);
        if v.t != element_type {
          panic!("expected element of type {} and found type {}, at {}",
            element_type, v.t, e.loc);
        }
        element_values.push(v.id);
      }
      let t = types::array_type(element_type, elements.len() as u64);
      let e = Expr::Array(perm_slice_from_vec(element_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // array length
    Command("array_len", [e]) => {
      let v = compile_expr_to_var(b, *e);
      let t = types::type_as_array(&v.t).expect("expected array");
      let e = Expr::LiteralU64(t.length);
      Some(push_expr(b, e, b.env.c.u64_tag).to_ref())
    }
    // array index
    Command("index", [v, index]) => {
      let v = compile_expr_to_ref(b, *v);
      let ptr = {
        if let Some(info) = types::type_as_array(&v.t) {
          let array_ptr = v.get_address(b);
          let element_ptr_type = types::pointer_type(info.inner);
          compile_cast(b, array_ptr, element_ptr_type)
        }
        else if v.t.kind == Kind::Pointer {
          v.to_var(b)
        }
        else {
          panic!("expected pointer or array")
        }
      };
      let offset = compile_expr_to_var(b, *index).id;
      let e = Expr::PtrOffset { ptr: ptr.id, offset };
      let ptr = push_expr(b, e, ptr.t);
      Some(pointer_to_locator(ptr, v.mutable))
    }
    // init
    Command("init", ns) => {
      let t = node_to_type(b, ns[0]);
      let field_nodes = &ns[1..];
      let mut field_values = vec![];
      for f in field_nodes {
        // TODO: check types
        field_values.push(compile_expr_to_var(b, *f).id);
      }
      let e = Expr::Init(perm_slice_from_vec(field_values));
      Some(push_expr(b, e, t).to_ref())
    }
    // field deref
    Command(".", [v, field]) => {
      let struct_ref = compile_and_fully_deref(b, *v);
      let struct_addr = struct_ref.get_address(b).id;
      let info = types::type_as_struct(&struct_ref.t).expect("expected struct");
      let i = match field.content {
        NodeContent::Sym(name) => {
          info.field_names.as_slice().iter().position(|n| *n == name)
            .expect("no such field") as u64
        }
        NodeContent::Literal(NodeLiteral::U64(i)) => i,
        _ => panic!("invalid field name"),
      };
      let field_type = info.field_types[i as usize];
      let e = Expr::FieldIndex{ struct_addr, index: i };
      let v = push_expr(b, e, types::pointer_type(field_type));
      Some(pointer_to_locator(v, struct_ref.mutable))
    }
    // template
    Command("#", [quoted]) => {
      Some(compile_template(b, *quoted).to_ref())
    }
    // quotation
    Command("quote", [quoted]) => {
      Some(compile_quote(b, *quoted).to_ref())
    }
    // def
    Command("def", [def_name, value]) => {
      let nb = NodeBuilder { loc: node.loc, st: b.env.st };
      let def_node = def_macro(&nb, *def_name, *value);
      compile_expr(b, def_node)
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let f = compile_function_def(b.env, arg_nodes.children(), None, *body);
      let v = function_to_var(b, f);
      Some(v.to_ref())
    }
    Command("fun", [arg_nodes, return_tag, body]) => {
      let f = compile_function_def(b.env, arg_nodes.children(), Some(*return_tag), *body);
      let v = function_to_var(b, f);
      Some(v.to_ref())
    }
    // macro
    Command("macro", [arg, body]) => {
      let f = compile_macro_def(b.env, *arg, *body);
      let v = function_to_var(b, f);
      let mv = compile_cast(b, v, types::macro_type(v.t));
      Some(mv.to_ref())
    }
    // set var
    Command("set", [dest, value]) => {
      let dest = compile_expr_to_ref(b, *dest);
      let value = compile_expr_to_var(b, *value);
      compile_assignment(b, dest, value);
      None
    }
    // let
    Command("let", [var_name, value]) => {
      // TODO: this generates redundant bytecode. If the value is a
      // locator, it should just generate a load, rather than a load and a store.
      let name = var_name.as_symbol();
      // evaluate the expression
      let value = compile_expr_to_var(b, *value);
      let local_var = new_local_variable(b, name, value.t);
      compile_assignment(b, local_var.to_ref(), value);
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_var(b, *cond).id;
      b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_expr(b, *then_expr);
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      compile_if_else(b, *cond, *then_expr, *else_expr)
    }
    // label expression
    Command("label", [label, body]) => {
      let name = label.as_symbol();
      let entry_seq = create_sequence(b, "block_entry");
      let exit_seq = create_sequence(b, "block_exit");
      b.bc.instrs.push(Instr::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledExpr{ name, entry_seq, exit_seq }
      );
      let result = compile_expr(b, *body);
      b.label_stack.pop();
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result.map(|x| x)
    }
    Command("do", exprs) => {
      let result = compile_block_expr(b, exprs);
      result.map(|x| x.to_ref())
    }
  // Debug
    Command("debug", [n]) => {
      let local = compile_expr_to_var(b, *n);
      b.bc.instrs.push(Instr::Debug(*n, local.id, local.t));
      None
    }
    // Return
    Command("return", [n]) => {
      let r = compile_expr_to_var(b, *n);
      b.bc.instrs.push(Instr::Return(Some(r.id)));
      None
    }
    // symbol
    Command("sym", [n]) => {
      let s = n.as_symbol();
      let e = Expr::LiteralU64(s.as_u64());
      return Some(push_expr(b, e, b.env.c.u64_tag).to_ref());
    }
    // typeof
    Command("typeof", [n]) => {
      // TODO: it's weird to codegen the expression when we only need its type
      let v = compile_expr_to_var(b, *n);
      let e = Expr::LiteralU64(Perm::to_u64(v.t));
      return Some(push_expr(b, e, b.env.c.type_tag).to_ref());
    }
    // load
    Command("*", [pointer]) => {
      let ptr = compile_expr_to_var(b, *pointer);
      Some(pointer_to_locator(ptr, true))
    }
    // ref
    Command("ref", [locator]) => {
      let local = compile_expr_to_ref(b, *locator);
      return Some(local.get_address(b).to_ref())
    }
    // cast
    Command("cast", [value, type_tag]) => {
      let t = node_to_type(b, *type_tag);
      let v = compile_expr_to_var(b, *value);
      Some(compile_cast(b, v, t).to_ref())
    }
    _ => {
      if let Some(type_tag) = try_node_to_type(b, node) {
        let e = Expr::LiteralU64(Perm::to_u64(type_tag));
        return Some(push_expr(b, e, b.env.c.type_tag).to_ref());
      }
      else {
        compile_list_expr(b, node)
      }
    }
  }
}

fn compile_literal(b : &mut Builder, l : NodeLiteral) -> Ref {
  match l {
    NodeLiteral::U64(v) => {
      let e = Expr::LiteralU64(v);
      push_expr(b, e, b.env.c.u64_tag).to_ref()
    }
    NodeLiteral::String(s) => {
      let t = types::pointer_type(b.env.c.string_tag);
      let e = Expr::Literal(t, Perm::to_ptr(s) as *const ());
      let v = push_expr(b, e, t);
      pointer_to_locator(v, false)
    }
  }
}

fn symbol_to_type(b : &Builder, s : Symbol) -> Option<TypeHandle> {
  env::get_global_value(b.env, s, b.env.c.type_tag)
}

fn parse_args(b: &Builder, arg_nodes : Node) -> Vec<TypeHandle> {
  let mut args = vec![];
  for &arg in arg_nodes.children() {
    let t =
      if let [_name, t] = arg.children() { *t }
      else { arg };
    args.push(node_to_type(b, t));
  }
  args
}

fn try_node_to_type(b: &Builder, n : Node) -> Option<TypeHandle> {
  match node_shape(&n) {
    Atom(s) => {
      symbol_to_type(b, to_symbol(b.env.st, s))
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      let inner = node_to_type(b, *inner_type);
      Some(types::pointer_type(inner))
    }
    // function type
    Command("fn", [arg_nodes, ret]) => {
      let args = parse_args(b, *arg_nodes);
      let returns = node_to_type(b, *ret);
      Some(types::function_type(&args, returns))
    }
    // c function type
    Command("cfun", [arg_nodes, ret]) => {
      let args = parse_args(b, *arg_nodes);
      let returns = node_to_type(b, *ret);
      Some(types::c_function_type(&args, returns))
    }
    // array type
    Command("sized_array", [element, size]) => {
      let element = node_to_type(b, *element);
      let size = size.as_literal_u64();
      Some(types::array_type(element, size))
    }
    // tuple type
    Command("tuple", fields) => {
      let mut field_types = vec![];
      for f in fields {
        field_types.push(node_to_type(b, *f));
      }
      Some(types::tuple_type(perm_slice_from_vec(field_types)))
    }
    // struct type
    Command("struct", fields) => {
      let mut field_names = vec![];
      let mut field_types = vec![];
      for &f in fields {
        if let [name, type_tag] = f.children() {
          field_names.push(name.as_symbol());
          field_types.push(node_to_type(b, *type_tag));
        }
        else {
          panic!("expected name/type pair")
        }
      }
      Some(types::struct_type(
        perm_slice_from_vec(field_names),
        perm_slice_from_vec(field_types)))
    }
    _ => None,
  }
}

fn node_to_type(b: &Builder, n : Node) -> TypeHandle {
  if let Some(t) = try_node_to_type(b, n) { t }
  else { panic!("{} is not valid type", n) }
}

fn compile_block_expr(b : &mut Builder, nodes : &[Node]) -> Option<Var> {  
  let mut v = None;
  let num_locals = b.scoped_vars.len();
  for &c in nodes {
    v = compile_expr(b, c);
  }
  b.scoped_vars.drain(num_locals..).for_each(|_| ());
  v.map(|v| v.to_var(b))
}

fn compile_symbol_reference(b : &mut Builder, node : Node) -> Ref {
  let sym = node.as_symbol();
  // Look for variable in local scope
  if let Some(r) = find_var_in_scope(b, sym) {
    return r.to_ref();
  }
  // Assume global
  if let Some(tag) = env::get_global_type(b.env, sym) {
    let e = Expr::Def(sym);
    let v = push_expr(b, e, types::pointer_type(tag));
    return pointer_to_locator(v, true);
  }
  panic!("symbol '{}' not defined ({})", sym, node.loc)
}

fn compile_function_call(b : &mut Builder, list : &[Node]) -> Var {
  let function = list[0];
  let args = &list[1..];
  let f = compile_expr_to_var(b, function);
  let info = if let Some(i) = types::type_as_function(&f.t) {
    i
  }
  else {
    panic!("expected function, found {} expression '{}' at ({})",
      f.t, function, function.loc);
  };
  let mut arg_values = vec![];
  for i in 0..args.len() {
    let arg = args[i];
    let v = compile_expr_to_var(b, arg);
    if info.args[i] != v.t  {
      panic!("expected arg of type {}, found type {}, at ({})", info.args[i], v.t, arg.loc);
    }
    if info.c_function && v.t.size_of > 8 {
      panic!("types passed to a C function must be 64 bits wide or less; found type {} of width {} as ({})",
        v.t, v.t.size_of, arg.loc);
    }
    arg_values.push(v.id);
  }
  let e = {
    if info.c_function {
      Expr::InvokeC(f.id, perm_slice_from_vec(arg_values))
    }
    else {
      Expr::Invoke(f.id, perm_slice_from_vec(arg_values))
    }
  };
  return push_expr(b, e, info.returns);
}

fn compile_macro_call(b : &mut Builder, f : &Function, n : Node) -> Option<Ref> {
  let nodes = &n.children()[1..];
  // macros actually take one argument, which is a node slice.
  // node_slice matches the layout of the two u64 args below.
  let args = &[nodes.as_ptr() as u64, nodes.len() as u64];
  let v = interpret::interpret_function(f, args, b.env);
  let new_node = Perm { p: v as *mut NodeInfo };
  compile_expr(b, new_node)
}

fn compile_call(b : &mut Builder, n : Node) -> Option<Ref> {
  let list = n.children();
  let function = list[0];
  if let parse::NodeContent::Sym(f) = function.content {
    if let Some(tag) = env::get_global_type(b.env, f) {
      if types::type_as_macro(&tag).is_some() {
        let f = env::get_global_value(b.env, f, tag).unwrap();
        return compile_macro_call(b, f, n);
      }
    }
  }
  Some(compile_function_call(b, list).to_ref())
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
      Add | Sub | Mul | Div | Rem => {
        if types::is_number(a) {
          return Some(a);
        }
      }
      Eq | LT | GT | LTE | GTE => {
        if types::is_number(a) || types::is_bool(a) {
          return Some(c.bool_tag);
        }
      }
      Not => (),
    }
  }
  None
}

fn unary_op_type(c : &CoreTypes, op : Operator, t : TypeHandle) -> Option<TypeHandle> {
  use Operator::*;
  if op == Not && t == c.bool_tag {
    return Some(c.bool_tag);
  }
  if op == Sub {
    if t == c.u64_tag {
      return Some(t);
    }
  }
  None
}

/// compiles a literal with an explicit type tag. For example:
///   (50 u32)
///   (30 i64)
fn compile_typed_literal(b : &mut Builder, node : Node) -> Option<Var> {
  if let [lit, tag] = node.children() {
    if let NodeContent::Literal(l) = lit.content {
      if let Some(t) = try_node_to_type(b, *tag) {
        let v = compile_literal(b, l).to_var(b);
        return Some(compile_cast(b, v, t))
      }
    }
  }
  None
}

fn compile_list_expr(b : &mut Builder, node : Node) -> Option<Ref> {
  if node.children().len() == 0 {
    return None;
  }
  if let Some(v) = compile_typed_literal(b, node) {
    return Some(v.to_ref());
  }
  match node_shape(&node) {
    Command(head, tail) => {
      if let Some(op) = str_to_operator(head) {
        if let [v1, v2] = tail {
          let v1 = compile_expr_to_var(b, *v1);
          let v2 = compile_expr_to_var(b, *v2);
          if let Some(t) = binary_op_type(&b.env.c, op, v1.t, v2.t) {
            let e = Expr::BinaryOp(op, v1.id, v2.id);
            return Some(push_expr(b, e, t).to_ref());
          }
          panic!("no binary op {} for types {} and {} at ({})",
            op, v1.t, v2.t, node.loc)
        }
        if let [v1] = tail {
          let v1 = compile_expr_to_var(b, *v1);
          if let Some(t) = unary_op_type(&b.env.c, op, v1.t) {
            let e = Expr::UnaryOp(op, v1.id);
            return Some(push_expr(b, e, t).to_ref());
          }
          panic!("no unary op {} for type {} at ({})",
            op, v1.t, node.loc)
        }
        panic!("incorrect number of args to operator {} at ({})", op, node.loc)
      }
    }
    _ => (),
  }
  compile_call(b, node)
}

fn compile_template(b : &mut Builder, quoted : Node) -> Var {
  
  fn find_template_arguments(n : Node, args : &mut Vec<Node>) {
    match node_shape(&n) {
      Command("$", [e]) => {
        args.push(*e);
      }
      _ => (),
    }
    for &c in n.children() {
      find_template_arguments(c, args);
    }
  }

  let mut template_args = vec![];
  find_template_arguments(quoted, &mut template_args);
  if template_args.len() > 0 {
    let nb = NodeBuilder { loc: quoted.loc, st: b.env.st };
    let n = template_macro(&nb, quoted, template_args.as_slice());
    compile_expr_to_var(b, n)
  }
  else {
    compile_quote(b, quoted)
  }
}

fn compile_quote(b : &mut Builder, quoted : Node) -> Var {
  let e = Expr::LiteralU64(Perm::to_ptr(quoted) as u64);
  let node_tag = b.env.c.node_tag;
  push_expr(b, e, node_tag)
}

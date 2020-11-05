/// Compiles core language into bytecode

use crate::{bytecode, parse, symbols, env, perm_alloc, types, interpret};

use bytecode::{
  SequenceId, SequenceInfo, Expr, FunctionBytecode,
  Instr, Operator, LocalId, LocalInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use perm_alloc::{Perm, perm, perm_slice, perm_slice_from_vec};

use symbols::{to_symbol, Symbol, SymbolTable};
use env::Env;
use parse::{
  Node, NodeInfo, NodeContent, NodeLiteral,
  node_shape, NodeShape, SrcLocation,
};
use NodeShape::*;

struct LabelledBlockExpr {
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
  scoped_locals : Vec<NamedLocal>,
  
  /// the instruction sequence currently being built
  cur_seq : Option<SequenceId>,

  /// flags indicating which sequences have been finalised
  seq_completion : Vec<bool>,

  /// labels indicating the start and end of a labelled block expression
  label_stack : Vec<LabelledBlockExpr>,

  /// the environment containing all visible symbols
  env : Env,
}

#[derive(Copy, Clone)]
enum VarType {
  /// A locator register contains a pointer to a value.
  /// Must be derefenced into value register to be read from.
  /// Can be written to.
  Locator(LocalId),

  /// A value register can be read from directly, but can't be written to.
  Value(LocalId),
}

#[derive(Copy, Clone)]
pub struct Var {
  var_type : VarType,
  data_type : TypeHandle,
  mutable : bool,
}

impl Var {

  fn from_val(val : Val) -> Self {
    Var{ var_type: VarType::Value(val.id), data_type: val.data_type, mutable: val.mutable }
  }

  fn get_address(&self, b : &mut Builder) -> Val {
    let t = types::pointer_type(self.data_type);
    let id = match self.var_type {
      VarType::Locator(id) => id,
      VarType::Value(id) => {
        let e = Expr::LocalAddr(id);
        let addr_id = new_local(b, None, t, self.mutable);
        b.bc.instrs.push(Instr::Expr(addr_id, e));
        addr_id
      }
    };
    Val { id, data_type: t, mutable: self.mutable }
  }
  
  fn to_val(&self, b : &mut Builder) -> Val {
    match self.var_type {
      VarType::Locator(r) => {
        let e = Expr::Load(r);
        push_expr(b, e, self.data_type)
      }
      VarType::Value(r) => Val {
        id: r,
        data_type: self.data_type,
        mutable: self.mutable,
      },
    }
  }
}

#[derive(Copy, Clone)]
struct Val {
  id : LocalId,
  data_type : TypeHandle,
  mutable : bool,
}

impl Val {
  fn to_var(&self) -> Var {
    Var::from_val(*self)
  }
}

#[derive(Copy, Clone)]
struct NamedLocal {
  name : Symbol,
  v : Val,
}

fn find_local_in_scope(b : &mut Builder, name : Symbol)
  -> Option<Val>
{
  let result =
    b.scoped_locals.iter().rev()
    .find(|&l| l.name == name).cloned();
  if let Some(nl) = result {
    return Some(nl.v);
  }
  None
}

fn new_scoped_var(b : &mut Builder, name : Symbol, t : TypeHandle) -> Val {
  let id = new_local(b, Some(name), t, true);
  let v = Val { id, data_type: t, mutable: true };
  let tl = NamedLocal{ name, v };
  b.scoped_locals.push(tl);
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

fn new_val(b : &mut Builder, t : TypeHandle, mutable : bool) -> Val {
  let id = new_local(b, None, t, mutable);
  Val { id, data_type: t, mutable }
}

fn push_expr(b : &mut Builder, e : Expr, t : TypeHandle) -> Val {
  let v = new_val(b, t, false);
  b.bc.instrs.push(Instr::Expr(v.id, e));
  return v;
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
  b.cur_seq = Some(sequence);
  b.bc.sequence_info[sequence.0].start_instruction = b.bc.instrs.len();
}

fn complete_sequence(b : &mut Builder) {
  if let Some(i) = b.cur_seq {
    let seq = &mut b.bc.sequence_info[i.0];
    seq.num_instructions = b.bc.instrs.len() - seq.start_instruction;
    b.seq_completion[i.0] = true;
    b.cur_seq = None;
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

fn compile_function(env: Env, args : &[Node], body : &[Node]) -> Function {
  let mut b = Builder {
    bc: FunctionBytecode {
      sequence_info: vec![],
      instrs: vec![],
      args: 0,
      locals: vec![],
      frame_bytes: 0,
    },
    scoped_locals: vec![],
    seq_completion: vec![],
    cur_seq: None,
    label_stack: vec![],
    env,
  };
  b.bc.args = args.len();
  let mut arg_types = vec![];
  for &arg in args {
    let (name, t) = {
      if let [type_tag, name] = arg.children() {
        let t =
          TypeHandle::from_u64(
            b.env.get(type_tag.as_symbol()).unwrap().value);
        (name.as_symbol(), t)
      }
      else {
        // TODO: fix type
        (arg.as_symbol(), env.c.u64_tag)
      }
    };
    new_scoped_var(&mut b, name, t);
    arg_types.push(t);
  }
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);
  let v = compile_block_expr(&mut b, body);
  let mut return_type = b.env.c.void_tag;
  if let Some(v) = v {
    return_type = v.data_type;
    b.bc.instrs.push(Instr::Return(Some(v.id)));
  }
  else {
    b.bc.instrs.push(Instr::Return(None));
  }
  let t = types::function_type(&arg_types, return_type); // TODO: fix arg types!
  let f = complete_function(b);
  // for n in body { println!("{}", n) }
  // println!("{}", f);
  Function { bc: f, t }
}

/// Compile basic imperative language into bytecode
pub fn compile_expr_to_function(env: Env, root : Node) -> Function {
  compile_function(env, &[], &[root])
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> Option<Var> {
  let mut result_local = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_val(b, cond).id;
  b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result = compile_expr(b, then_expr);
  if let Some(v) = then_result {
    let l = new_val(b, v.data_type, true).to_var();
    result_local = Some(l);
    let v = v.to_val(b);
    compile_set_local(b, l, v);
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result = compile_expr(b, else_expr);
  if let Some(l) = result_local {
    let v = else_result.expect("expected block expression").to_val(b);
    compile_set_local(b, l, v);
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  result_local
}

fn compile_set_local(b : &mut Builder, dest : Var, value : Val) {
  if dest.data_type.size_of != value.data_type.size_of {
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

fn compile_expr_to_var(b : &mut Builder, node : Node) -> Var {
  if let Some(var) = compile_expr(b, node) {
    var
  }
  else {
    panic!("expected value, found none, at node {} ({})", node, node.loc);
  }
}

fn compile_expr_to_val(b : &mut Builder, node : Node) -> Val {
  let var = compile_expr_to_var(b, node);
  var.to_val(b)
}

fn compile_bitcast(b : &mut Builder, v : Val, t : TypeHandle) -> Val {
  if v.data_type.size_of != t.size_of {
    panic!("can't cast {} to {}", v.data_type, t)
  }
  push_expr(b, Expr::BitCopy(v.id), t)
}

fn compile_expr(b : &mut Builder, node : Node) -> Option<Var> {
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
      Some(push_expr(b, e, b.env.c.u64_tag).to_var())
    }
    // false
    Atom("false") => {
      let e = Expr::LiteralU64(0);
      Some(push_expr(b, e, b.env.c.u64_tag).to_var())
    }
    // symbol reference (local var or global def)
    Atom(_) => {
      Some(compile_symbol_reference(b, node))
    }
    // literal
    Literal(l) => {
      match l {
        NodeLiteral::U64(v) => {
          let e = Expr::LiteralU64(v);
          Some(push_expr(b, e, b.env.c.u64_tag).to_var())
        }
        NodeLiteral::String(s) => {
          let e = Expr::Literal(b.env.c.string_tag, s.ptr as *const ());
          Some(push_expr(b, e, b.env.c.string_tag).to_var())
        }
      }
    }
    // array
    Command("array", elements) => {
      let e = compile_expr_to_val(b, elements[0]);
      let element_type = e.data_type;
      let mut element_values = vec![e.id];
      for e in &elements[1..] {
        let v = compile_expr_to_val(b, *e);
        // TODO: check type
        element_values.push(v.id);
      }
      let t = types::array_type(element_type, elements.len() as u64);
      let e = Expr::Array(perm_slice_from_vec(element_values));
      Some(push_expr(b, e, t).to_var())
    }
    // array index
    Command("index", [v, index]) => {
      let array_val = compile_expr_to_var(b, *v);
      let array_ptr = array_val.get_address(b);
      let info = types::type_as_array(&array_val.data_type).expect("expected array");
      let element_ptr_type = types::pointer_type(info.inner);
      let ptr = compile_bitcast(b, array_ptr, element_ptr_type).id;
      let offset = compile_expr_to_val(b, *index).id;
      let e = Expr::PtrOffset { ptr, offset };
      let id = new_local(b, None, element_ptr_type, array_val.mutable);
      b.bc.instrs.push(Instr::Expr(id, e));
      Some(Var { var_type: VarType::Locator(id), data_type: info.inner, mutable: array_val.mutable})
    }
    // init
    Command("init", ns) => {
      let t = node_to_type(b, ns[0]);
      let field_nodes = &ns[1..];
      let mut field_values = vec![];
      for f in field_nodes {
        // TODO: check types
        field_values.push(compile_expr_to_val(b, *f).id);
      }
      let e = Expr::Init(perm_slice_from_vec(field_values));
      Some(push_expr(b, e, t).to_var())
    }
    // field deref
    Command(".", [v, field]) => {
      let struct_val = compile_expr_to_var(b, *v);
      let struct_addr = struct_val.get_address(b).id;
      let info = types::type_as_struct(&struct_val.data_type).expect("expected struct");
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
      let id = new_local(b, None, types::pointer_type(field_type), struct_val.mutable);
      b.bc.instrs.push(Instr::Expr(id, e));
      Some(Var { var_type: VarType::Locator(id), data_type: field_type, mutable: struct_val.mutable})
    }
    // quotation
    Command("#", [quoted]) => {
      // Some(quote(b, *quoted).to_var())
      panic!()
    }
    // def
    Command("def", [def_name, value]) => {
      let nb = NodeBuilder { loc: node.loc, st: b.env.st };
      let def_node = def_macro(&nb, *def_name, *value);
      compile_expr(b, def_node)
    }
    // fun
    Command("fun", [arg_nodes, return_type, body]) => {
      let f = compile_function(
        b.env, arg_nodes.children(), &[*body]);
      let r = new_val(b, f.t, false);
      let f_addr = Box::into_raw(Box::new(f)) as u64;
      b.bc.instrs.push(Instr::Expr(r.id, Expr::LiteralU64(f_addr)));
      Some(r.to_var())
    }
    // set var
    Command("set", [dest, value]) => {
      let dest = compile_expr_to_var(b, *dest);
      let value = compile_expr_to_val(b, *value);
      compile_set_local(b, dest, value);
      None
    }
    // let
    Command("let", [var_name, value]) => {
      let name = var_name.as_symbol();
      // evaluate the expression
      let value = compile_expr_to_val(b, *value);
      let local = new_scoped_var(b, name, value.data_type);
      compile_set_local(b, local.to_var(), value);
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_val(b, *cond).id;
      b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_block_expr(b, then_expr.children());
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      None
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      compile_if_else(b, *cond, *then_expr, *else_expr)
    }
    // block expression
    Command("block", [body]) => {
      let entry_seq = create_sequence(b, "block_entry");
      let exit_seq = create_sequence(b, "block_exit");
      b.bc.instrs.push(Instr::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledBlockExpr{ entry_seq, exit_seq }
      );
      let result = compile_block_expr(b, body.children());
      b.label_stack.pop();
      b.bc.instrs.push(Instr::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result.map(|x| x.to_var())
    }
    Command("do", exprs) => {
      let result = compile_block_expr(b, exprs);
      result.map(|x| x.to_var())
    }
  // Debug
    Command("debug", [n]) => {
      let val = compile_expr_to_val(b, *n);
      b.bc.instrs.push(Instr::Debug(*n, val.id, val.data_type));
      None
    }
    // Return
    Command("return", [n]) => {
      let r = compile_expr_to_val(b, *n);
      b.bc.instrs.push(Instr::Return(Some(r.id)));
      None
    }
    // symbol
    Command("sym", [n]) => {
      let s = n.as_symbol();
      let e = Expr::LiteralU64(s.as_u64());
      return Some(push_expr(b, e, b.env.c.u64_tag).to_var());
    }
    // typeof
    Command("typeof", [n]) => {
      let v = compile_expr_to_val(b, *n);
      let e = Expr::LiteralU64(Perm::to_u64(v.data_type));
      return Some(push_expr(b, e, b.env.c.type_tag).to_var());
    }
    // load
    Command("*", [pointer]) => {
      let ptr = compile_expr_to_val(b, *pointer);
      let t =
        types::deref_pointer_type(&ptr.data_type)
        .expect("expected pointer type");
      let var = Var{ var_type: VarType::Locator(ptr.id), data_type: t, mutable: true };
      return Some(var);
    }
    // ref
    Command("ref", [locator]) => {
      let val = compile_expr_to_var(b, *locator);
      return Some(val.get_address(b).to_var())
    }
    // cast
    Command("cast", [value, type_tag]) => {
      let t = node_to_type(b, *type_tag);
      let v = compile_expr_to_val(b, *value);
      Some(compile_bitcast(b, v, t).to_var())
    }
    _ => {
      if let Some(type_tag) = try_node_to_type(b, node) {
        let e = Expr::LiteralU64(Perm::to_u64(type_tag));
        return Some(push_expr(b, e, b.env.c.type_tag).to_var());
      }
      else {
        compile_list_expr(b, node)
      }
    }
  }
}

fn symbol_to_type(b : &Builder, s : Symbol) -> Option<TypeHandle> {
  if let Some(entry) = b.env.get(s) {
    if entry.tag.kind == Kind::Type {
      return Some(TypeHandle::from_u64(entry.value));
    }
  }
  None
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

fn compile_block_expr(b : &mut Builder, nodes : &[Node]) -> Option<Val> {  
  let mut v = None;
  let num_locals = b.scoped_locals.len();
  for &c in nodes {
    v = compile_expr(b, c);
  }
  b.scoped_locals.drain(num_locals..).for_each(|_| ());
  v.map(|v| v.to_val(b))
}

fn compile_symbol_reference(b : &mut Builder, node : Node) -> Var {
  let sym = node.as_symbol();
  // Look for local
  if let Some(r) = find_local_in_scope(b, sym) {
    return r.to_var();
  }
  // Assume global
  if let Some(entry) = b.env.get(sym) {
    let e = Expr::Def(sym);
    let t = entry.tag;
    return push_expr(b, e, t).to_var();
  }
  panic!("symbol '{}' not defined ({})", sym, node.loc)
}

fn compile_function_call(b : &mut Builder, list : &[Node]) -> Val {
  let function = list[0];
  let args = &list[1..];
  let f = compile_expr_to_val(b, function);
  let info = if let Some(i) = types::type_as_function(&f.data_type) {
    i
  }
  else {
    panic!("expected function, found {} expression '{}' at ({})",
      f.data_type, function, function.loc);
  };
  let mut arg_values = vec![];
  for &arg in args {
    // TODO: check types
    let v = compile_expr_to_val(b, arg);
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

fn compile_macro_call(b : &mut Builder, f : &Function, n : Node) -> Option<Var> {
  let args = n.perm_children().slice_range(1..);
  let node = perm(NodeInfo{
    loc: n.loc,
    content: NodeContent::List(args),
  });
  let v = interpret::interpret_function(f, &[Perm::to_ptr(node) as u64], b.env);
  let new_node = Perm { p: v as *mut NodeInfo };
  compile_expr(b, new_node)
}

fn compile_call(b : &mut Builder, n : Node) -> Option<Var> {
  let list = n.children();
  let function = list[0];
  if let parse::NodeContent::Sym(f) = function.content {
    if let Some(e) = b.env.get(f) {
      if types::type_as_macro(&e.tag).is_some() {
        let f = unsafe { &*(e.value as *const Function) };
        return compile_macro_call(b, f, n);
      }
    }
  }
  Some(compile_function_call(b, list).to_var())
}

fn str_to_operator(s : &str) -> Option<Operator> {
  use Operator::*;
  let op = match s {
    "+" => Add,
    "-" => Sub,
    "*" => Mul,
    "/" => Div,
    "%" => Rem,
    "=" => Eq,
    "<" => LT,
    ">" => GT,
    "<=" => LTE,
    ">=" => GTE,
    "!" => Not,
    _ => return None,
  };
  Some(op)
}

fn op_result_type(c : &CoreTypes, op : Operator) -> TypeHandle {
  use Operator::*;
  match op {
    Add | Sub | Mul | Div | Rem |
    Eq | LT | GT | LTE | GTE |
    Not => c.u64_tag,
  }
}

fn compile_list_expr(b : &mut Builder, node : Node) -> Option<Var> {
  if node.children().len() == 0 {
    return None;
  }
  match node_shape(&node) {
    Command(head, tail) => {
      if let Some(op) = str_to_operator(head) {
        let t = op_result_type(&b.env.c, op);
        if let [v1, v2] = tail {
          let v1 = compile_expr_to_val(b, *v1);
          let v2 = compile_expr_to_val(b, *v2);
          let e = Expr::BinaryOp(op, v1.id, v2.id);
          return Some(push_expr(b, e, t).to_var());
        }
        if let [v1] = tail {
          let v1 = compile_expr_to_val(b, *v1);
          let e = Expr::UnaryOp(op, v1.id);
          return Some(push_expr(b, e, t).to_var());
        }
      }
    }
    _ => (),
  }
  compile_call(b, node)
}

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

/// Handles templating if necessary
// TODO: reimplement with NodeBuilder!
// fn quote(b : &mut Builder, quoted : Node) -> Val {
//   let u64_tag = b.env.c.u64_tag;
//   let node_value = {
//     let e = Expr::LiteralU64(Perm::to_ptr(quoted) as u64);
//     push_expr(b, e, u64_tag)
//   };
//   let mut template_args = vec![];
//   find_template_arguments(quoted, &mut template_args);
//   if template_args.len() > 0 {
//     use Operator::*;
//     let args = template_args.len() as u64;
//     let data_ptr = {
//       let t = types::array_type(&b.env.c, args * 8);
//       let v = new_val(b, t, true);
//       v.to_var().get_address(b)
//     };
//     // Codegen args
//     let mut arg_values = vec![];
//     for a in template_args {
//       arg_values.push(compile_expr_to_val(b, a));
//     }
//     // Store args
//     for (i, a) in arg_values.iter().enumerate() {
//       let pointer = {
//         let offset = push_expr(b, Expr::LiteralU64(i as u64 * 8), u64_tag);
//         push_expr(b, Expr::BinaryOp(Add, data_ptr.id, offset.id), u64_tag).id
//       };
//       b.bc.instrs.push(Instr::Store{ pointer, value: a.id });
//     }
//     // slice struct
//     let slice_ptr = {
//       let v = new_val(b, b.env.c.slice_tag, true);
//       v.to_var().get_address(b)
//     };
//     // store slice length
//     let slice_length = push_expr(b, Expr::LiteralU64(args), u64_tag);
//     b.bc.instrs.push(Instr::Store{
//       pointer: slice_ptr.id,
//       value: slice_length.id,
//     });
//     // store slice data
//     let data_pointer = {
//       let eight = push_expr(b, Expr::LiteralU64(8), u64_tag);
//       push_expr(b, Expr::BinaryOp(Add, slice_ptr.id, eight.id), u64_tag)
//     };
//     b.bc.instrs.push(Instr::Store{
//       pointer: data_pointer.id,
//       value: data_ptr.id,
//     });
//     // template quote reference
//     let f = {
//       let template_quote = to_symbol(b.env.st, "template_quote");
//       push_expr(b, Expr::Def(template_quote), u64_tag)
//     };
//     // call template_quote
//     let args = perm_slice(&[node_value.id, slice_ptr.id]);
//     push_expr(b, Expr::InvokeC(f.id, args), u64_tag)
//   }
//   else {
//     node_value
//   }
// }

struct NodeBuilder {
  loc : SrcLocation,
  st : SymbolTable,
}

impl NodeBuilder {
  fn list(&self, ns : &[Node]) -> Node {
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::List(perm_slice(ns)),
    })
  }

  fn sym(&self, s : &str) -> Node {
    let sym = to_symbol(self.st, s);
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::Sym(sym),
    })
  }
}

fn def_macro(b : &NodeBuilder, def_name : Node, value : Node) -> Node {
  b.list(&[
    b.sym("do"),
    b.list(&[b.sym("let"), b.sym("v"), value]),
    b.list(&[
      b.sym("env_insert"),
      b.sym("env"),
      b.list(&[b.sym("sym"), def_name]),
      b.sym("v"),
      b.list(&[b.sym("typeof"), b.sym("v")])])
  ])
}

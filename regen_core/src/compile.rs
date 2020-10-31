/// Compiles core language into bytecode

use crate::{bytecode, parse, symbols, env, perm_alloc, types, interpret};

use bytecode::{
  SequenceId, SequenceInfo, Expr, FunctionBytecode,
  Instr, Operator, LocalId, LocalInfo, RegId, RegisterInfo,
};

use types::{TypeHandle, CoreTypes, Kind};

use perm_alloc::{Perm, perm, perm_slice, perm_slice_from_vec};

use symbols::{to_symbol, Symbol, SymbolTable};
use env::Env;
use parse::{
  Node, NodeInfo, NodeContent,
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
  scoped_locals : Vec<TypedLocal>,
  
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
  Locator(RegId),

  /// A value register can be read from directly, but can't be written to.
  Value(RegId),
}

#[derive(Copy, Clone)]
pub struct Var {
  var_type : VarType,
  data_type : TypeHandle,
}

impl Var {

  fn from_val(val : Val) -> Self {
    Var{ var_type: VarType::Value(val.r), data_type: val.data_type }
  }

  fn get_address(&self) -> Val {
    match self.var_type {
      VarType::Locator(r) => Val {
        r,
        data_type: types::pointer_type(self.data_type)
      },
      VarType::Value(_) => panic!("this expression has no address"),
    }
  }
  
  fn to_val(&self, b : &mut Builder) -> Val {
    match self.var_type {
      VarType::Locator(r) => {
        let e = Expr::Load(r);
        push_expr(b, e, self.data_type)
      }
      VarType::Value(r) => Val { r, data_type: self.data_type },
    }
  }
}

#[derive(Copy, Clone)]
struct Val {
  r : RegId,
  data_type : TypeHandle,
}

impl Val {
  fn to_var(&self) -> Var {
    Var::from_val(*self)
  }
}

#[derive(Copy, Clone)]
struct TypedLocal {
  l : LocalId,
  name : Option<Symbol>,
  t : TypeHandle,
}

fn to_local(r : RegId, data_type : TypeHandle) -> Var {
  Var { var_type: VarType::Locator(r), data_type }
}

fn find_local_in_scope(b : &mut Builder, name : Symbol)
  -> Option<Var>
{
  let r =
    b.scoped_locals.iter().rev()
    .find(|&l| l.name == Some(name)).cloned();
  if let Some(tl) = r {
    return Some(local_to_var(b, tl));
  }
  None
}

fn local_to_var(b : &mut Builder, l : TypedLocal) -> Var {
  let e = Expr::LocalId(l.l);
  let r = new_register(b, l.t);
  let var = Var { var_type: VarType::Locator(r), data_type: l.t };
  b.bc.instrs.push(Instr::Expr(r, e));
  var
}

fn add_local(b : &mut Builder, name : Option<Symbol>, t : TypeHandle) -> TypedLocal {
  let id = LocalId { id: 0 };
  let info = LocalInfo {
    id,
    name,
    byte_offset: 0,
    t,
  };
  b.bc.locals.push(info);
  let tl = TypedLocal{ l: id, name, t};
  b.scoped_locals.push(tl);
  tl
}

fn new_register(b : &mut Builder, t : TypeHandle) -> RegId {
  let id = RegId { id: b.bc.registers.len() };
  let info = RegisterInfo { id, byte_offset: 0, t };
  b.bc.registers.push(info);
  id
}

fn new_val(b : &mut Builder, t : TypeHandle) -> Val {
  let r = new_register(b, t);
  Val { r, data_type: t }
}

fn new_anonymous_local(b : &mut Builder, t : TypeHandle) -> Var {
  let l = add_local(b, None, t);
  local_to_var(b, l)
}

fn push_expr(b : &mut Builder, e : Expr, t : TypeHandle) -> Val {
  let v = new_val(b, t);
  b.bc.instrs.push(Instr::Expr(v.r, e));
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
  for r in b.bc.registers.as_mut_slice() {
    r.byte_offset = b.bc.frame_bytes;
    let bytes = types::round_up_multiple(r.t.size_of, 8);
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
      registers: vec![],
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
    add_local(&mut b, Some(name), t);
    arg_types.push(t);
  }
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);
  let v = compile_block_expr(&mut b, body);
  let mut return_type = b.env.c.void_tag;
  if let Some(v) = v {
    return_type = v.data_type;
    b.bc.instrs.push(Instr::Return(Some(v.r)));
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
  let cond = compile_expr_to_val(b, cond).r;
  b.bc.instrs.push(Instr::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result =
    compile_block_expr(b, then_expr.children());
  if let Some(v) = then_result {
    let l = new_anonymous_local(b, v.data_type);
    result_local = Some(l);
    compile_set_local(b, l, v);
  }
  b.bc.instrs.push(Instr::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result =
    compile_block_expr(b, else_expr.children());
  if let Some(l) = result_local {
    let v = else_result.expect("expected block expression");
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
  let pointer = dest.get_address().r;
  b.bc.instrs.push(Instr::Store {
    pointer,
    value: value.r,
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

fn compile_expr(b : &mut Builder, node : Node) -> Option<Var> {
  match node_shape(&node) {
    // Return
    Atom("return") => {
      b.bc.instrs.push(Instr::Return(None));
      None
    }
    // Break
    Atom("break") => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.bc.instrs.push(Instr::Jump(break_to));
      None
    }
    // Repeat
    Atom("repeat") => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.bc.instrs.push(Instr::Jump(loop_back_to));
      None
    }
    Atom(_) => {
      Some(atom_to_value(b, node))
    }
    // Literal
    Literal(v) => {
      let e = Expr::LiteralU64(v);
      Some(push_expr(b, e, b.env.c.u64_tag).to_var())
    }
    // quotation
    Command("#", [quoted]) => {
      Some(quote(b, *quoted).to_var())
    }
    // def
    Command("def", [def_name, value]) => {
      let nb = NodeBuilder { loc: node.loc, st: b.env.st };
      let def_node = def_macro(&nb, *def_name, *value);
      compile_expr(b, def_node)
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let f = compile_function(
        b.env, arg_nodes.children(), body.children());
      let r = new_val(b, f.t);
      let f_addr = Box::into_raw(Box::new(f)) as u64;
      b.bc.instrs.push(Instr::Expr(r.r, Expr::LiteralU64(f_addr)));
      Some(r.to_var())
    }
    // set var
    Command("set", [dest, value]) => {
      let dest = compile_expr_to_var(b, *dest);
      let value = compile_expr_to_val(b, *value);
      compile_set_local(b, dest, value);
      None
    }
    // store var
    Command("store", [pointer, value]) => {
      let pointer = compile_expr_to_val(b, *pointer).r;
      let value = compile_expr_to_val(b, *value).r;
      b.bc.instrs.push(Instr::Store{ pointer, value });
      None
    }
    // stack allocate
    Command("byte_chunk", [value]) => {
      let array = types::array_type(&b.env.c, value.as_literal());
      Some(new_val(b, array).to_var())
    }
    // let
    Command("let", [var_name, value]) => {
      let name = var_name.as_symbol();
      // evaluate the expression
      let value = compile_expr_to_val(b, *value);
      let local = add_local(b, Some(name), value.data_type);
      let dest = local_to_var(b, local);
      compile_set_local(b, dest, value);
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_val(b, *cond).r;
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
      b.bc.instrs.push(Instr::Debug(*n, val.r, val.data_type));
      None
    }
    // Return
    Command("return", [n]) => {
      let r = compile_expr_to_val(b, *n);
      b.bc.instrs.push(Instr::Return(Some(r.r)));
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
      let var = Var{ var_type: VarType::Locator(ptr.r), data_type: t};
      return Some(var);
    }
    // ref
    Command("ref", [locator]) => {
      let val = compile_expr_to_var(b, *locator);
      return Some(val.get_address().to_var())
    }
    // cast
    Command("cast", [value, type_tag]) => {
      let v = compile_expr_to_val(b, *value);
      let t = node_to_type(b, *type_tag).unwrap();
      if v.data_type.size_of != t.size_of {
        panic!("can't cast {} to {}", v.data_type, t)
      }
      let cv = Val { r: v.r, data_type: t };
      Some(cv.to_var())
    }
    _ => {
      if let Some(type_tag) = node_to_type(b, node) {
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

fn node_to_type(b: &Builder, n : Node) -> Option<TypeHandle> {
  match node_shape(&n) {
    Atom(s) => {
      symbol_to_type(b, to_symbol(b.env.st, s))
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      let inner = node_to_type(b, *inner_type).unwrap();
      Some(types::pointer_type(inner))
    }
    _ => None,
  }
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

fn atom_to_value(b : &mut Builder, node : Node) -> Var {
  let sym = node.as_symbol();
  // boolean literals
  match sym.as_str() {
    "true" => {
      let e = Expr::LiteralU64(1);
      return push_expr(b, e, b.env.c.u64_tag).to_var();
    }
    "false" => {
      let e = Expr::LiteralU64(0);
      return push_expr(b, e, b.env.c.u64_tag).to_var();
    }
    _ => (),
  }
  // Look for local
  if let Some(r) = find_local_in_scope(b, sym) {
    return r;
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
    let v = compile_expr_to_val(b, arg);
    arg_values.push(v.r);
  }
  let e = {
    if info.c_function {
      Expr::InvokeC(f.r, perm_slice_from_vec(arg_values))
    }
    else {
      Expr::Invoke(f.r, perm_slice_from_vec(arg_values))
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
          let e = Expr::BinaryOp(op, v1.r, v2.r);
          return Some(push_expr(b, e, t).to_var());
        }
        if let [v1] = tail {
          let v1 = compile_expr_to_val(b, *v1);
          let e = Expr::UnaryOp(op, v1.r);
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
fn quote(b : &mut Builder, quoted : Node) -> Val {
  let u64_tag = b.env.c.u64_tag;
  let node_value = {
    let e = Expr::LiteralU64(Perm::to_ptr(quoted) as u64);
    push_expr(b, e, u64_tag)
  };
  let mut template_args = vec![];
  find_template_arguments(quoted, &mut template_args);
  if template_args.len() > 0 {
    use Operator::*;
    let args = template_args.len() as u64;
    let data_ptr = {
      let t = types::array_type(&b.env.c, args * 8);
      new_anonymous_local(b, t).get_address()
    };
    // Codegen args
    let mut arg_values = vec![];
    for a in template_args {
      arg_values.push(compile_expr_to_val(b, a));
    }
    // Store args
    for (i, a) in arg_values.iter().enumerate() {
      let pointer = {
        let offset = push_expr(b, Expr::LiteralU64(i as u64 * 8), u64_tag);
        push_expr(b, Expr::BinaryOp(Add, data_ptr.r, offset.r), u64_tag).r
      };
      b.bc.instrs.push(Instr::Store{ pointer, value: a.r });
    }
    // slice struct
    let slice_ptr = new_anonymous_local(b, b.env.c.slice_tag).get_address();
    // store slice length
    let slice_length = push_expr(b, Expr::LiteralU64(args), u64_tag);
    b.bc.instrs.push(Instr::Store{
      pointer: slice_ptr.r,
      value: slice_length.r,
    });
    // store slice data
    let data_pointer = {
      let eight = push_expr(b, Expr::LiteralU64(8), u64_tag);
      push_expr(b, Expr::BinaryOp(Add, slice_ptr.r, eight.r), u64_tag)
    };
    b.bc.instrs.push(Instr::Store{
      pointer: data_pointer.r,
      value: data_ptr.r,
    });
    // template quote reference
    let f = {
      let template_quote = to_symbol(b.env.st, "template_quote");
      push_expr(b, Expr::Def(template_quote), u64_tag)
    };
    // call template_quote
    let args = perm_slice(&[node_value.r, slice_ptr.r]);
    push_expr(b, Expr::InvokeC(f.r, args), u64_tag)
  }
  else {
    node_value
  }
}

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
    b.sym("env_insert"),
    b.sym("env"),
    b.list(&[b.sym("sym"), def_name]),
    value, b.sym("u64")])
}

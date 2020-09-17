/// Compiles core language into bytecode

use crate::{bytecode, parse, symbols, env, perm_alloc, types, interpret};

use bytecode::{
  SequenceId, SequenceInfo, Expr, FunctionBytecode,
  NamedVar, Op, Operator, FrameVar,
};

use types::{TypeHandle, CoreTypes};

use perm_alloc::{Perm, perm};

use symbols::to_symbol;
use env::Env;
use parse::{
  Node, NodeInfo, NodeContent,
  node_shape, NodeShape,
};
use NodeShape::*;
use symbols::Symbol;

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
  /// number of arguments the function takes
  args : usize,

  /// named local variables (including the arguments)
  locals : Vec<NamedVar>,
  
  /// local variables in scope
  scoped_locals : Vec<(NamedVar, TypeHandle)>,
  
  /// registers
  registers : Vec<FrameVar>,

  /// frame size in multiple of 64bit words
  frame_bytes : u64,

  /// the instruction sequence currently being built
  cur_seq : Option<SequenceId>,

  /// flags indicating which sequences have been finalised
  seq_completion : Vec<bool>,

  /// sequence information
  seq_info : Vec<SequenceInfo>,

  /// the operations of all the sequences
  ops : Vec<Op>,

  /// labels indicating the start and end of a labelled block expression
  label_stack : Vec<LabelledBlockExpr>,

  /// the environment containing all visible symbols
  env : Env,
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum VarType {
  Local, Register
}

use VarType::*;

#[derive(Copy, Clone)]
struct Var {
  fv : FrameVar,
  var_type : VarType,
  data_type : TypeHandle,
}

fn to_local(var : FrameVar, data_type : TypeHandle) -> Var {
  Var { fv: var, var_type: Local, data_type }
}

fn find_local_in_scope(b : &mut Builder, name : Symbol)
  -> Option<Var>
{
  b.scoped_locals.iter().rev()
    .find(|&l| l.0.name == name)
    .map(|(l, t)| Var { fv: l.var, var_type: Local, data_type: *t})
}

fn add_local(b : &mut Builder, name : Symbol, var : FrameVar, t : TypeHandle) -> Var {
  let local = NamedVar{ name, var };
  b.locals.push(local);
  b.scoped_locals.push((local, t));
  Var { fv: var, var_type: Local, data_type: t }
}

fn new_frame_var(b : &mut Builder, minimum_bytes : u64) -> FrameVar {
  let bytes = types::round_up_multiple(minimum_bytes as u64, 8);
  let id = b.registers.len();
  let var = FrameVar{ id, byte_offset: b.frame_bytes as u32, bytes: bytes as u32 };
  b.registers.push(var);
  b.frame_bytes += bytes;
  var
}

fn new_var(b : &mut Builder, t : TypeHandle) -> Var {
  let var = new_frame_var(b, t.size_of);
  Var { fv: var, var_type: Register, data_type: t }
}

fn push_expr(b : &mut Builder, e : Expr, t : TypeHandle) -> Var {
  let r = new_var(b, t);
  b.ops.push(Op::Expr(r.fv, e));
  return r;
}

fn create_sequence(b : &mut Builder, name : &str) -> SequenceId {
  // Make sure the name is unique
  let mut i = 1;
  let mut name_candidate = symbols::to_symbol(b.env.st, name);
  loop {
    let name_unique = b.seq_info.iter().find(|s| s.name == name_candidate).is_none();
    if name_unique { break }
    i += 1;
    name_candidate = symbols::to_symbol(b.env.st, &format!("{}_{}", name, i));
  }
  let seq_id = SequenceId(b.seq_info.len());
  b.seq_info.push(SequenceInfo {
    name: name_candidate,
    start_op: 0, num_ops: 0,
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
  b.seq_info[sequence.0].start_op = b.ops.len();
}

fn complete_sequence(b : &mut Builder) {
  if let Some(i) = b.cur_seq {
    let seq = &mut b.seq_info[i.0];
    seq.num_ops = b.ops.len() - seq.start_op;
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
  FunctionBytecode {
    sequence_info: b.seq_info,
    ops: b.ops,
    frame_bytes: b.frame_bytes,
    args: b.args,
    locals: b.locals,
    registers: b.registers,
  }
}

fn compile_function(env: Env, args : &[Node], body : &[Node]) -> Function {
  let mut b = Builder {
    args: 0,
    locals: vec![],
    scoped_locals: vec![],
    registers: vec![],
    frame_bytes: 0,
    seq_completion: vec![],
    cur_seq: None,
    seq_info: vec![],
    ops: vec![],
    label_stack: vec![],
    env,
  };
  b.args = args.len();
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
    let var = new_frame_var(&mut b, t.size_of);
    add_local(&mut b, name, var, t);
    arg_types.push(t);
  }
  // start a sequence
  let entry_seq = create_sequence(&mut b, "entry");
  set_current_sequence(&mut b, entry_seq);
  let v = compile_block_expr(&mut b, body);
  let mut return_type = b.env.c.void_tag;
  if let Some(v) = v {
    return_type = v.data_type;
    b.ops.push(Op::Return(Some(v.fv)));
  }
  else {
    b.ops.push(Op::Return(None));
  }
  let t = types::function_type(&b.env.c, &arg_types, return_type); // TODO: fix arg types!
  let f = complete_function(b);
  //for n in body { println!("{}", n) }
  //println!("{}", f);
  Function { bc: f, t }
}

/// Compile basic imperative language into bytecode
pub fn compile_expr_to_function(env: Env, root : Node) -> Function {
  compile_function(env, &[], &[root])
}

fn compile_if_else(b : &mut Builder, cond : Node, then_expr : Node, else_expr : Node) -> Option<Var> {
  let mut result_reg = None;
  let then_seq = create_sequence(b, "then");
  let else_seq = create_sequence(b, "else");
  let exit_seq = create_sequence(b, "exit");
  let cond = compile_expr_to_value(b, cond).fv;
  b.ops.push(Op::CJump{ cond, then_seq, else_seq });
  set_current_sequence(b, then_seq);
  let then_result =
    compile_block_expr(b, then_expr.children());
  if let Some(v) = then_result {
    let r = new_var(b, v.data_type);
    result_reg = Some(r);
    b.ops.push(Op::Set(r.fv, v.fv));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, else_seq);
  let else_result =
    compile_block_expr(b, else_expr.children());
  if let Some(r) = result_reg {
    let v = else_result.expect("expected block expression");
    b.ops.push(Op::Set(r.fv, v.fv));
  }
  b.ops.push(Op::Jump(exit_seq));
  set_current_sequence(b, exit_seq);
  result_reg
}

fn compile_expr_to_value(b : &mut Builder, node : Node) -> Var {
  if let Some(v) = compile_expr(b, node) {
    v
  }
  else {
    panic!("expected value, found none, at node {} ({})", node, node.loc);
  }
}

fn compile_expr(b : &mut Builder, node : Node) -> Option<Var> {
  match node_shape(&node) {
    // Return
    Atom("return") => {
      b.ops.push(Op::Return(None));
      None
    }
    // Break
    Atom("break") => {
      let break_to = b.label_stack.last().unwrap().exit_seq;
      b.ops.push(Op::Jump(break_to));
      None
    }
    // Repeat
    Atom("repeat") => {
      let loop_back_to = b.label_stack.last().unwrap().entry_seq;
      b.ops.push(Op::Jump(loop_back_to));
      None
    }
    Atom(_) => {
      Some(atom_to_value(b, node))
    }
    // Literal
    Literal(v) => {
      let e = Expr::LiteralU64(v);
      Some(push_expr(b, e, b.env.c.u64_tag))
    }
    // quotation
    Command("#", [quoted]) => {
      Some(quote(b, *quoted))
    }
    // def
    Command("def", [def_name, value]) => {
      def_macro(b, *def_name, *value);
      None
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let f = compile_function(
        b.env, arg_nodes.children(), body.children());
      let r = new_var(b, f.t);
      let f_addr = Box::into_raw(Box::new(f)) as u64;
      b.ops.push(Op::Expr(r.fv, Expr::LiteralU64(f_addr)));
      Some(r)
    }
    // set var
    Command("set", [varname, value]) => {
      let value = compile_expr_to_value(b, *value);
      let variable =
        find_local_in_scope(b, varname.as_symbol())
        .expect("no variable found");
      if value.data_type.size_of != variable.data_type.size_of {
        panic!("types don't match")
      }
      b.ops.push(Op::Set(variable.fv, value.fv));
      None
    }
    // store var
    Command("store", [pointer, value]) => {
      let pointer = compile_expr_to_value(b, *pointer).fv;
      let value = compile_expr_to_value(b, *value).fv;
      b.ops.push(Op::Store{ byte_width: value.bytes as u64, pointer, value });
      None
    }
    // store var explicit type
    Command("store", [type_tag, pointer, value]) => {
      let e = b.env.get(type_tag.as_symbol()).unwrap();
      let t = TypeHandle::from_u64(e.value);
      let pointer = compile_expr_to_value(b, *pointer).fv;
      let value = compile_expr_to_value(b, *value).fv;
      b.ops.push(Op::Store{ byte_width: t.size_of, pointer, value });
      None
    }
    // stack allocate
    Command("byte_chunk", [value]) => {
      let array = types::array_type(&b.env.c, value.as_literal());
      Some(new_var(b, array))
    }
    // let
    Command("let", [var_name, value]) => {
      let name = var_name.as_symbol();
      // evaluate the expression
      let val = compile_expr_to_value(b, *value);
      match val.var_type {
        Local => {
          let var = new_var(b, val.data_type).fv;
          add_local(b, name, var, val.data_type);
          b.ops.push(Op::Set(var, val.fv));
        }
        Register => {
          add_local(b, name, val.fv, val.data_type);
        }
      }
      None
    }
    // if then
    Command("if", [cond, then_expr]) => {
      let then_seq = create_sequence(b, "then");
      let exit_seq = create_sequence(b, "exit");
      let cond = compile_expr_to_value(b, *cond).fv;
      b.ops.push(Op::CJump{ cond, then_seq, else_seq: exit_seq });
      set_current_sequence(b, then_seq);
      compile_block_expr(b, then_expr.children());
      b.ops.push(Op::Jump(exit_seq));
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
      b.ops.push(Op::Jump(entry_seq));
      set_current_sequence(b, entry_seq);
      b.label_stack.push(
        LabelledBlockExpr{ entry_seq, exit_seq }
      );
      let result = compile_block_expr(b, body.children());
      b.label_stack.pop();
      b.ops.push(Op::Jump(exit_seq));
      set_current_sequence(b, exit_seq);
      result
    }
    Command("do", exprs) => {
      let result = compile_block_expr(b, exprs);
      result
    }
  // Debug
    Command("debug", [v]) => {
      let r = compile_expr_to_value(b, *v);
      let sym = to_symbol(b.env.st, parse::code_segment(&*v.loc.code, *v));
      b.ops.push(Op::Debug(sym, r.fv));
      None
    }
    // Return
    Command("return", [v]) => {
      let r = compile_expr_to_value(b, *v);
      b.ops.push(Op::Return(Some(r.fv)));
      None
    }
    // symbol
    Command("sym", [v]) => {
      let s = v.as_symbol();
      let e = Expr::LiteralU64(s.as_u64());
      return Some(push_expr(b, e, b.env.c.u64_tag));
    }
    // typeof
    Command("typeof", [v]) => {
      let v = compile_expr_to_value(b, *v);
      let e = Expr::LiteralU64(Perm::to_u64(v.data_type));
      return Some(push_expr(b, e, b.env.c.u64_tag));
    }
    // load
    Command("load", [type_tag, pointer]) => {
      let entry = b.env.get(type_tag.as_symbol()).unwrap();
      let t = TypeHandle::from_u64(entry.value);
      let ptr = compile_expr_to_value(b, *pointer).fv;
      let e = Expr::Load{ bytes: t.size_of, ptr };
      return Some(push_expr(b, e, t));
    }
    _ => {
      compile_list_expr(b, node)
    }
  }
}

fn compile_block_expr(b : &mut Builder, nodes : &[Node]) -> Option<Var> {  
  let mut v = None;
  let num_locals = b.scoped_locals.len();
  for &c in nodes {
    v = compile_expr(b, c);
  }
  b.scoped_locals.drain(num_locals..).for_each(|_| ());
  v
}

fn atom_to_value(b : &mut Builder, node : Node) -> Var {
  let sym = node.as_symbol();
  // boolean literals
  match sym.as_str() {
    "true" => {
      let e = Expr::LiteralU64(1);
      return push_expr(b, e, b.env.c.u64_tag);
    }
    "false" => {
      let e = Expr::LiteralU64(0);
      return push_expr(b, e, b.env.c.u64_tag);
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
    return push_expr(b, e, t);
  }
  panic!("symbol '{}' not defined ({})", sym, node.loc)
}

fn compile_function_call(b : &mut Builder, list : &[Node]) -> Var {
  let function = list[0];
  let args = &list[1..];
  let f = compile_expr_to_value(b, function);
  let env = b.env;
  let info = if let Some(i) = env.c.as_function(&f.data_type) {
    i
  }
  else {
    panic!("expected function, found {} expression '{}' at ({})",
      f.data_type.kind, function, function.loc);
  };
  let mut arg_values = vec![]; // TODO: remove allocation
  for &arg in args {
    let v = compile_expr_to_value(b, arg);
    arg_values.push(v.fv);
  }
  let mut byte_offset = 0;
  for value in arg_values.drain(..) {
    // TODO: check arg values
    b.ops.push(Op::Arg{ byte_offset, value });
    byte_offset += types::round_up_multiple(value.bytes as u64, 8);
  }
  let e = {
    if info.c_function {
      Expr::InvokeC(f.fv, args.len())
    }
    else {
      Expr::Invoke(f.fv)
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
      if b.env.c.as_macro(&e.tag).is_some() {
        let f = unsafe { &*(e.value as *const Function) };
        return compile_macro_call(b, f, n);
      }
    }
  }
  Some(compile_function_call(b, list))
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
    "ref" => Ref,
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
    Ref | Not => c.u64_tag,
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
          let v1 = compile_expr_to_value(b, *v1);
          let v2 = compile_expr_to_value(b, *v2);
          let e = Expr::BinaryOp(op, v1.fv, v2.fv);
          return Some(push_expr(b, e, t));
        }
        if let [v1] = tail {
          let v1 = compile_expr_to_value(b, *v1);
          let e = Expr::UnaryOp(op, v1.fv);
          return Some(push_expr(b, e, t));
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
fn quote(b : &mut Builder, quoted : Node) -> Var {
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
      let data = new_var(b, t);
      push_expr(b, Expr::UnaryOp(Ref, data.fv), u64_tag)
    };
    // Codegen args
    let mut arg_values = vec![];
    for a in template_args {
      arg_values.push(compile_expr_to_value(b, a));
    }
    // Store args
    for (i, a) in arg_values.iter().enumerate() {
      let pointer = {
        let offset = push_expr(b, Expr::LiteralU64(i as u64 * 8), u64_tag);
        push_expr(b, Expr::BinaryOp(Add, data_ptr.fv, offset.fv), u64_tag).fv
      };
      b.ops.push(Op::Store{ byte_width: 8, pointer, value: a.fv });
    }
    // slice struct
    let slice_var = new_var(b, b.env.c.slice_tag);
    let slice_ptr = push_expr(b, Expr::UnaryOp(Ref, slice_var.fv), u64_tag);
    // store slice length
    let slice_length = push_expr(b, Expr::LiteralU64(args), u64_tag);
    b.ops.push(Op::Store{
      byte_width: 8,
      pointer: slice_ptr.fv,
      value: slice_length.fv,
    });
    // store slice data
    let data_pointer = {
      let eight = push_expr(b, Expr::LiteralU64(8), u64_tag);
      push_expr(b, Expr::BinaryOp(Add, slice_ptr.fv, eight.fv), u64_tag)
    };
    b.ops.push(Op::Store{
      byte_width: 8,
      pointer: data_pointer.fv,
      value: data_ptr.fv,
    });
    // template quote reference
    let f = {
      let template_quote = to_symbol(b.env.st, "template_quote");
      push_expr(b, Expr::Def(template_quote), u64_tag)
    };
    // call template_quote
    b.ops.push(Op::Arg{byte_offset: 0, value: node_value.fv}); // quoted node
    b.ops.push(Op::Arg{byte_offset: 8, value: slice_ptr.fv}); // slice reference
    push_expr(b, Expr::InvokeC(f.fv, 2), u64_tag)
  }
  else {
    node_value
  }
}

/// TODO: this is long-winded. replace it with an in-language macro!
fn def_macro(b : &mut Builder, def_name : Node, value : Node) {
  use Expr::*;
  // evaluate the expression
  let val_reg = compile_expr_to_value(b, value);
  // call env insert
  let env = {
    let env = to_symbol(b.env.st, "env");
    let v = new_frame_var(b, 8);
    b.ops.push(Op::Expr(v, Def(env)));
    v
  };
  let def_sym = {
    let def = def_name.as_symbol().as_u64();
    let v = new_frame_var(b, 8);
    b.ops.push(Op::Expr(v, LiteralU64(def)));
    v
  };
  let type_tag = {
    let t = Perm::to_u64(val_reg.data_type); // TODO: this is very inefficient
    let v = new_frame_var(b, 8);
    b.ops.push(Op::Expr(v, LiteralU64(t)));
    v
  };
  let f = {
    let env_insert = to_symbol(b.env.st, "env_insert");
    let f = new_frame_var(b, 8);
    b.ops.push(Op::Expr(f, Def(env_insert)));
    f
  };
  b.ops.push(Op::Arg{byte_offset: 0, value: env}); // env
  b.ops.push(Op::Arg{byte_offset: 8, value: def_sym}); // symbol name
  b.ops.push(Op::Arg{byte_offset: 16, value: val_reg.fv}); // value
  b.ops.push(Op::Arg{byte_offset: 24, value: type_tag}); // type
  let v = new_frame_var(b, 8);
  b.ops.push(Op::Expr(v, InvokeC(f, 4)));
}

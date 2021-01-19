
use crate::{sexp, symbols, node_macros, perm_alloc, ffi_libs, bytecode};
use sexp::{
  Node,
  NodeLiteral,
  NodeShape::*,
  NodeContent,
};
use node_macros::{NodeBuilder, def_macro, template_macro};
use symbols::{Symbol, SymbolTable};
use perm_alloc::{Perm, PermSlice, perm, perm_slice_from_vec};
use ffi_libs::RegenString;
use bytecode::Operator;

use std::collections::HashSet;

pub type Expr = Perm<ExprData>;

/// TODO: unused
// #[derive(Copy, Clone)]
// pub struct ExprType {
//   t : TypeHandle,
//   const_expr : bool,
//   is_locator : bool,
//   mutable : bool,
// }

#[derive(Copy, Clone)]
pub struct ExprData {
  pub content : ExprContent,
  pub n : Node,
}

#[derive(Copy, Clone)]
pub enum ExprContent {
  Let(Node, Expr),
  DefMarker { name: Node, initialiser : Expr },
  LocalRef(Symbol),
  GlobalRef(Symbol),
  StructInit(Expr, PermSlice<Expr>),
  ZeroInit(Expr),
  ArrayInit(PermSlice<Expr>),
  ArrayIndex { array : Expr, index : Expr },
  ArrayAsSlice(Expr),
  PtrIndex { ptr : Expr, index : Expr },
  FieldIndex { structure : Expr, field_name : Node },
  LiteralU64(u64),
  LiteralString(Perm<RegenString>),
  LiteralBool(bool),
  LiteralVoid,
  Call(Expr, PermSlice<Expr>),
  InstrinicOp(Operator, PermSlice<Expr>),
  Cast{ value : Expr, to_type : Expr },
  IfElse { cond : Expr, then_expr : Expr, else_expr : Option<Expr>},
  Debug(Expr),
  Sym(Symbol),
  Set{ dest : Expr, value : Expr },
  Deref(Expr),
  GetAddress(Expr),
  Return(Option<Expr>),
  Break(Option<Symbol>),
  Repeat(Option<Symbol>),
  LabelledBlock(Symbol, Expr),
  Do(PermSlice<Expr>),
  Quote(Node),
  Fun {
    args : PermSlice<(Symbol, Expr)>,
    ret : Option<Expr>,
    body: Expr,
  },
  ArrayLen(Expr),
  TypeOf(Expr),
  FnType {
    args : PermSlice<Arg>,
    ret : Expr,
  },
  CFunType {
    args : PermSlice<Arg>,
    ret : Expr,
  },
  StructType(PermSlice<Arg>),
  PtrType(Expr),
  SizedArrayType{ element_type: Expr, length: Expr },
}

#[derive(Copy, Clone)]
pub struct Arg {
  pub name : Option<Node>,
  pub tag : Expr,
}

use ExprContent::*;

struct TagState {
  locals : Vec<Symbol>,
  st : SymbolTable,
  globals_referenced : HashSet<Symbol>,
}

#[derive(Copy, Clone)]
pub struct TaggedNode {
  pub tag : ExprContent,
  pub n : Node,
}

pub fn parse_to_expr(st : SymbolTable, root : Node) -> Expr {
  parse_to_expr_with_global_references(st, root).0
}

pub fn parse_to_expr_with_global_references(st : SymbolTable, root : Node) -> (Expr, HashSet<Symbol>) {
  let mut ts = TagState {
    locals: vec![],
    st,
    globals_referenced: HashSet::new(),
  };
  let e = to_expr(&mut ts, root);
  (e, ts.globals_referenced)
}

fn to_expr(ts : &mut TagState, n : Node) -> Expr {
  let ed = ExprData {
    content: to_expr_content(ts, n),
    n,
  };
  perm(ed)
}

fn to_expr_content(ts : &mut TagState, n : Node) -> ExprContent {
  match sexp::node_shape(&n) {
    // symbol reference (local var or global def)
    Atom(s) => {
      match s {
        "return" => Return(None),
        "break" => Break(None),
        "repeat" => Repeat(None),
        "true" => LiteralBool(true),
        "false" => LiteralBool(false),
        _ => {
          let sym = n.as_symbol();
          if ts.locals.contains(&sym) {
            LocalRef(sym)
          }
          else {
            ts.globals_referenced.insert(sym);
            GlobalRef(sym)
          }
        }
      }
    }
    // literal
    Literal(l) => {
      match l {
        NodeLiteral::U64(v) => {
          LiteralU64(v)
        }
        NodeLiteral::String(s) => {
          LiteralString(s)
        }
      }
    }
    // break to label
    Command("break", [label]) => {
      Break(Some(label.as_symbol()))
    }
    // repeat to label
    Command("repeat", [label]) => {
      Repeat(Some(label.as_symbol()))
    }
    // array
    Command("array", elements) => {
      ArrayInit(to_expr_list(ts, elements))
    }
    // array length
    Command("array_len", [e]) => {
      ArrayLen(to_expr(ts, *e))
    }
    // array to slice
    Command("as_slice", [e]) => {
      ArrayAsSlice(to_expr(ts, *e))
    }
    // array index
    Command("index", [v, index]) => {
      ArrayIndex {
        array: to_expr(ts, *v),
        index: to_expr(ts, *index),
      }
    }
    // ptr index
    Command("ptr_index", [v, index]) => {
      PtrIndex {
        ptr: to_expr(ts, *v),
        index: to_expr(ts, *index),
      }
    }
    // slice type
    Command("slice_index", [v, index]) => {
      let nb = NodeBuilder { loc: n.loc, st: ts.st };
      let slice_node = node_macros::slice_index_macro(&nb, *v, *index);
      to_expr_content(ts, slice_node)
    }
    // init
    Command("zero_init", [type_node]) => {
      ZeroInit(to_expr(ts, *type_node))
    }
    // init
    Command("init", ns) => {
      StructInit(to_expr(ts, ns[0]), to_expr_list(ts, &ns[1..]))
    }
    // field deref
    Command(".", [v, field]) => {
      FieldIndex{
        structure: to_expr(ts, *v),
        field_name: *field,
      }
    }
    // template
    Command("#", [quoted]) => {
      to_template_expr(ts, *quoted)
    }
    // for
    Command("for", [loop_var, start, end, body]) => {
      let nb = NodeBuilder { loc: n.loc, st: ts.st };
      let for_node = node_macros::for_macro(&nb, *loop_var, *start, *end, *body);
      to_expr_content(ts, for_node)
    }
    // while
    Command("while", [cond, body]) => {
      let nb = NodeBuilder { loc: n.loc, st: ts.st };
      let while_node = node_macros::while_macro(&nb, *cond, *body);
      to_expr_content(ts, while_node)
    }
    // quotation
    Command("quote", [quoted]) => {
      Quote(*quoted)
    }
    // def
    Command("def", [def_name, value]) => {
      let nb = NodeBuilder { loc: n.loc, st: ts.st };
      let def_node = def_macro(&nb, *def_name, *value);
      DefMarker{ name: *def_name, initialiser: to_expr(ts, def_node) }
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let (args, body) = to_args_body(ts, arg_nodes.children(), *body);
      Fun{ args, ret: None, body }
    }
    Command("fun", [arg_nodes, return_tag, body]) => {
      let (args, body) = to_args_body(ts, arg_nodes.children(), *body);
      let ret = Some(to_expr(ts, *return_tag));
      Fun{ args, ret, body }
    }
    // set var
    Command("set", [dest, value]) => {
      Set{
        dest: to_expr(ts, *dest),
        value: to_expr(ts, *value),
      }
    }
    // let
    Command("let", [var_name, value]) => {
      ts.locals.push(var_name.as_symbol());
      Let(*var_name, to_expr(ts, *value))
    }
    // if then
    Command("if", [cond_node, then_expr]) => {
      IfElse {
        cond: to_expr(ts, *cond_node),
        then_expr: to_expr(ts, *then_expr),
        else_expr: None,
      }
    }
    // if then else
    Command("if", [cond_node, then_expr, else_expr]) => {
      IfElse {
        cond: to_expr(ts, *cond_node),
        then_expr: to_expr(ts, *then_expr),
        else_expr: Some(to_expr(ts, *else_expr)),
      }
    }
    // label expression
    Command("label", [label, body]) => {
      LabelledBlock(label.as_symbol(), to_expr(ts, *body))
    }
    Command("do", exprs) => {
      let locals = ts.locals.len();
      let ec = Do(to_expr_list(ts, exprs));
      ts.locals.drain(locals..);
      ec
    }
  // Debug
    Command("debug", [v]) => {
      Debug(to_expr(ts, *v))
    }
    // Return
    Command("return", [v]) => {
      Return(Some(to_expr(ts, *v)))
    }
    // symbol
    Command("sym", [v]) => {
      Sym(v.as_symbol())
    }
    // typeof
    Command("typeof", [v]) => {
      TypeOf(to_expr(ts, *v))
    }
    // load
    Command("*", [pointer]) => {
      Deref(to_expr(ts, *pointer))
    }
    // ref
    Command("ref", [locator]) => {
      GetAddress(to_expr(ts, *locator))
    }
    // cast
    Command("cast", [value, type_tag]) => {
      Cast {
        value: to_expr(ts, *value),
        to_type: to_expr(ts, *type_tag),
      }
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      PtrType(to_expr(ts, *inner_type))
    }
    // function type
    Command("fn", [arg_nodes, ret]) => {
      FnType {
        args: to_type_args_list(ts, arg_nodes.children()),
        ret: to_expr(ts, *ret),
      }
    }
    // c function type
    Command("cfun", [arg_nodes, ret]) => {
      CFunType {
        args: to_type_args_list(ts, arg_nodes.children()),
        ret: to_expr(ts, *ret),
      }
    }
    // array type
    Command("sized_array", [element, length]) => {
      SizedArrayType {
        element_type: to_expr(ts, *element),
        length: to_expr(ts, *length),
      }
    }
    // struct type
    Command("struct", fields) => {
      StructType(to_type_args_list(ts, fields))
    }
    // slice type
    Command("slice", [element_type]) => {
      let nb = NodeBuilder { loc: n.loc, st: ts.st };
      let slice_node = node_macros::slice_type_macro(&nb, *element_type);
      to_expr_content(ts, slice_node)
    }
    _ => {
      let ns = n.children();
      if ns.len() == 0 {
        return LiteralVoid;
      }
      if let [lit, tag] = ns {
        if let NodeContent::Literal(NodeLiteral::U64(_)) = lit.content {
          return Cast {
            value: to_expr(ts, *lit),
            to_type: to_expr(ts, *tag),
          };
        }
      }
      if let NodeContent::Sym(sym) = ns[0].content {
        if let Some(op) = str_to_operator(sym.as_str()) {
          return InstrinicOp(op, to_expr_list(ts, &ns[1..]));
        }
      }
      Call(to_expr(ts, ns[0]), to_expr_list(ts, &ns[1..]))
    }
  }
}

fn to_args_body(ts : &mut TagState, args : &[Node], body : Node) -> (PermSlice<(Symbol, Expr)>, Expr) {
  let mut v = Vec::with_capacity(args.len());
  for &a in args {
    if let [name, type_tag] = a.children() {
      let name = name.as_symbol();
      v.push((name, to_expr(ts, *type_tag)));
    }
    else {
      panic!()
    }
  }
  let args = perm_slice_from_vec(v);
  let locals = ts.locals.len();
  for (name, _) in args { ts.locals.push(*name); }
  let body = to_expr(ts, body);
  ts.locals.drain(locals..);
  (args, body)
}

fn to_type_args_list(ts : &mut TagState, args : &[Node]) -> PermSlice<Arg> {
  let mut v = Vec::with_capacity(args.len());
  for a in args {
    let arg = {
      if let [name, type_tag] = a.children() {
        name.as_symbol();
        Arg { name: Some(*name), tag: to_expr(ts, *type_tag) }
      }
      else if let [type_tag] = a.children() {
        Arg { name: None, tag: to_expr(ts, *type_tag) }
      }
      else {
        panic!("expected arg at ({})", a.loc)
      }
    };
    v.push(arg);
  }
  perm_slice_from_vec(v)
}

fn to_expr_list(ts : &mut TagState, ns : &[Node]) -> PermSlice<Expr> {
  let mut v = Vec::with_capacity(ns.len());
  for &n in ns {
    v.push(to_expr(ts, n));
  }
  perm_slice_from_vec(v)
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

fn to_template_expr(ts : &mut TagState, quoted : Node) -> ExprContent {
  
  fn find_template_arguments(n : Node, args : &mut Vec<Node>) {
    match sexp::node_shape(&n) {
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
    let nb = NodeBuilder { loc: quoted.loc, st: ts.st };
    let n = template_macro(&nb, quoted, template_args.as_slice());
    to_expr_content(ts, n)
  }
  else {
    Quote(quoted)
  }
}

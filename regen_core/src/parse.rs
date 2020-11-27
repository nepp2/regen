
use crate::{sexp, symbols, perm_alloc};
use sexp::{
  Node,
  NodeLiteral,
  NodeShape::*,
  NodeContent,
};
use symbols::Symbol;
use perm_alloc::Perm;

use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExprTag {
  Def,
  Let,
  LocalRef,
  GlobalRef,
  StructInit,
  ZeroInit,
  ArrayInit,
  Index,
  FieldIndex,
  LiteralU64,
  LiteralString,
  LiteralBool,
  LiteralVoid,
  LiteralType(Type),
  Call,
  Operator(IntrinsicOperator),
  Cast,
  IfElse,
  Debug,
  Symbol,
  Set,
  Deref,
  Ref,
  Return,
  Break,
  Repeat,
  LabelledBlock,
  Do,
  Template,
  Quote,
  Fun,
  Macro,
  ArrayLen,
  TypeOf,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
  Fn, CFun, Struct, Ptr, SizedArray,
}

use ExprTag::*;

struct TagState {
  info : NodeInfo,
  locals : Vec<Symbol>,
}

pub struct NodeInfo {
  pub tags : HashMap<u64, ExprTag>
}

pub fn tag_nodes(root : Node) -> NodeInfo {
  let mut ts = TagState {
    info: NodeInfo { tags: HashMap::new() },
    locals: vec![],
  };
  tag_node(&mut ts, root);
  return ts.info;
}

impl TagState {
  fn tag(&mut self, n : Node, tag : ExprTag) {
    self.info.tags.insert(Perm::to_u64(n), tag);
  }
}

fn tag_node(ts : &mut TagState, n : Node) {
  match sexp::node_shape(&n) {
    // symbol reference (local var or global def)
    Atom(s) => {
      let tag = match s {
        "return" => Return,
        "break" => Break,
        "repeat" => Repeat,
        "true" | "false" => LiteralBool,
        _ => {
          if let Some(i) = str_to_operator(s) {
            Operator(i)
          }
          else if ts.locals.contains(&n.as_symbol()) {
            LocalRef
          }
          else {
            GlobalRef
          }
        }
      };
      ts.tag(n, tag);
    }
    // literal
    Literal(l) => {
      match l {
        NodeLiteral::U64(_) => {
          ts.tag(n, LiteralU64);
        }
        NodeLiteral::String(_) => {
          ts.tag(n, LiteralString);
        }
      }
    }
    // break to label
    Command("break", [_label]) => {
      ts.tag(n, Break);
    }
    // repeat to label
    Command("repeat", [_label]) => {
      ts.tag(n, Repeat);
    }
    // array
    Command("array", elements) => {
      ts.tag(n, ArrayInit);
      tag_node_slice(ts, elements);
    }
    // array length
    Command("array_len", [e]) => {
      ts.tag(n, Call);
      tag_node(ts, *e);
    }
    // array index
    Command("index", [v, index]) => {
      ts.tag(n, Index);
      tag_node(ts, *v);
      tag_node(ts, *index);
    }
    // init
    Command("zero_init", [type_node]) => {
      ts.tag(n, ZeroInit);
      tag_node(ts, *type_node);
    }
    // init
    Command("init", ns) => {
      ts.tag(n, StructInit);
      tag_node_slice(ts, ns);
    }
    // field deref
    Command(".", [v, _field]) => {
      ts.tag(n, FieldIndex);
      tag_node(ts, *v);
    }
    // template
    Command("#", [_quoted]) => {
      ts.tag(n, Template);
    }
    // quotation
    Command("quote", [_quoted]) => {
      ts.tag(n, Quote);
    }
    // def
    Command("def", [_def_name, value]) => {
      ts.tag(n, Def);
      tag_node(ts, *value);
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      ts.tag(n, Fun);
      for &a in arg_nodes.children() {
        tag_typed_var(ts, a);
      }
      tag_node(ts, *body);
    }
    Command("fun", [arg_nodes, return_tag, body]) => {
      ts.tag(n, Fun);
      for &a in arg_nodes.children() {
        tag_typed_var(ts, a);
      }
      tag_node(ts, *return_tag);
      tag_node(ts, *body);
    }
    // macro
    Command("macro", [arg, body]) => {
      ts.tag(n, Macro);
      tag_node(ts, *arg);
      tag_node(ts, *body);
    }
    // set var
    Command("set", [dest, value]) => {
      ts.tag(n, Set);
      tag_node(ts, *dest);
      tag_node(ts, *value);
    }
    // let
    Command("let", [var_name, value]) => {
      ts.tag(n, Let);
      ts.locals.push(var_name.as_symbol());
      tag_node(ts, *value);
    }
    // if then
    Command("if", [cond_node, then_expr]) => {
      ts.tag(n, IfElse);
      tag_node(ts, *cond_node);
      tag_node(ts, *then_expr);
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      ts.tag(n, IfElse);
      tag_node(ts, *cond);
      tag_node(ts, *then_expr);
      tag_node(ts, *else_expr);
    }
    // label expression
    Command("label", [_label, body]) => {
      ts.tag(n, LabelledBlock);
      tag_node(ts, *body);
    }
    Command("do", exprs) => {
      ts.tag(n, Do);
      let locals = ts.locals.len();
      tag_node_slice(ts, exprs);
      ts.locals.drain(locals..);
    }
  // Debug
    Command("debug", [v]) => {
      ts.tag(n, Debug);
      tag_node(ts, *v);
    }
    // Return
    Command("return", [v]) => {
      ts.tag(n, Return);
      tag_node(ts, *v);
    }
    // symbol
    Command("sym", [v]) => {
      ts.tag(n, Symbol);
      tag_node(ts, *v);
    }
    // typeof
    Command("typeof", [v]) => {
      ts.tag(n, TypeOf);
      tag_node(ts, *v);
    }
    // load
    Command("*", [pointer]) => {
      ts.tag(n, Deref);
      tag_node(ts, *pointer);
    }
    // ref
    Command("ref", [locator]) => {
      ts.tag(n, Ref);
      tag_node(ts, *locator);
    }
    // cast
    Command("cast", [value, type_tag]) => {
      ts.tag(n, Cast);
      tag_node(ts, *value);
      tag_node(ts, *type_tag);
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      ts.tag(n, LiteralType(Type::Ptr));
      tag_node(ts, *inner_type);
    }
    // function type
    Command("fn", [arg_nodes, ret]) => {
      ts.tag(n, LiteralType(Type::Fn));
      tag_type_args(ts, arg_nodes.children());
      tag_node(ts, *ret);
    }
    // c function type
    Command("cfun", [arg_nodes, ret]) => {
      ts.tag(n, LiteralType(Type::CFun));
      tag_type_args(ts, arg_nodes.children());
      tag_node(ts, *ret);
    }
    // array type
    Command("sized_array", [element, size]) => {
      ts.tag(n, LiteralType(Type::SizedArray));
      tag_node(ts, *element);
      tag_node(ts, *size);
    }
    // struct type
    Command("struct", fields) => {
      ts.tag(n, LiteralType(Type::Struct));
      tag_type_args(ts, fields);
    }
    _ => {
      tag_list_expr(ts, n);
    }
  }
}

fn tag_typed_var(ts : &mut TagState, n : Node) {
  if let [name, type_tag] = n.children() {
    ts.locals.push(name.as_symbol());
    tag_node(ts, *type_tag);
  }
}

fn tag_type_args(ts : &mut TagState, args : &[Node]) {
  for a in args {
    if let [_name, type_tag] = a.children() {
      tag_node(ts, *type_tag);
    }
    else if let [type_tag] = a.children() {
      tag_node(ts, *type_tag);
    }
  }
}

fn tag_node_slice(ts : &mut TagState, ns : &[Node]) {
  for &n in ns { tag_node(ts, n); }
}

fn tag_list_expr(ts : &mut TagState, n : Node) {
  let ns = n.children();
  if ns.len() == 0 {
    ts.tag(n, LiteralVoid);
    return;
  }
  if let [lit, tag] = ns {
    if let NodeContent::Literal(NodeLiteral::U64(_)) = lit.content {
      ts.tag(n, Cast);
      tag_node(ts, *lit);
      tag_node(ts, *tag);
      return;
    }
  }
  ts.tag(n, Call);
  tag_node_slice(ts, ns);
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntrinsicOperator {
  // arithmetic
  Add, Sub, Mul, Div, Rem,
  // comparison
  Eq, LT, GT, LTE, GTE,
  // boolean
  Not, And, Or,
  // bitwise
  BitwiseNot, BitwiseAnd, BitwiseOr,
}


fn str_to_operator(s : &str) -> Option<IntrinsicOperator> {
  use IntrinsicOperator::*;
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
    "&&" => And,
    "||" => Or,
    _ => return None,
  };
  Some(op)
}

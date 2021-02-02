
use crate::{bytecode, ffi_libs, node_macros, perm_alloc, sexp, symbols};
use sexp::{
  Node,
  NodeLiteral,
  NodeShape::*,
  NodeContent,
  SrcLocation,
};
use node_macros::{NodeBuilder, template_macro};
use symbols::{Symbol, SymbolTable};
use perm_alloc::{Ptr, SlicePtr, perm, perm_slice, perm_slice_from_vec};
use ffi_libs::RegenString;
use bytecode::Operator;

pub type Expr = Ptr<ExprData>;

#[derive(Copy, Clone)]
pub struct ExprData {
  pub tag : ExprTag,
  pub content : ExprContent,
  pub loc : SrcLocation,
}

#[derive(Copy, Clone, PartialEq)]
pub enum ExprTag {
  Let,
  Reference,
  StructInit,
  ZeroInit,
  ArrayInit,
  ArrayIndex,
  ArrayAsSlice,
  PtrIndex,
  FieldIndex,
  LiteralExpr,
  Call,
  InstrinicOp,
  Cast,
  IfElse,
  Debug,
  Set,
  Deref,
  GetAddress,
  Return,
  Break,
  Repeat,
  LabelledBlock,
  Do,
  Fun,
  ArrayLen,
  TypeOf,
  FnType,
  CFunType,
  StructType,
  PtrType,
  SizedArrayType,
  Implicit, // was not specified
  Syntax, // cannot be evaluated
}

#[derive(Copy, Clone)]
pub enum Val {
  U64(u64),
  String(Ptr<RegenString>),
  Bool(bool),
  Symbol(Symbol),
  Node(Node),
  Void,
  Operator(Operator),
}

#[derive(Copy, Clone)]
pub enum ExprContent {
  List(SlicePtr<Expr>),
  LiteralVal(Val),
}

use ExprContent::*;

impl ExprData {
  pub fn shape(&self) -> ExprShape {
    match self.content {
      List(l) => ExprShape::List(self.tag, l.as_slice()),
      LiteralVal(v) => {
        if self.tag == ExprTag::Reference {
          if let Val::Symbol(s) = v {
            return ExprShape::Ref(s);
          }
        }
        return ExprShape::Literal(v);
      }
    }
  }

  pub fn children(&self) -> &[Expr] {
    if let List(es) = self.content {
      return es.as_slice();
    }
    &[]
  }

  pub fn as_syntax(&self) -> Option<&[Expr]> {
    if self.tag == Syntax {
      if let List(es) = self.content {
        return Some(es.as_slice());
      }
    }
    None
  }

  pub fn as_symbol_literal(&self) -> Symbol {
    if let LiteralVal(Val::Symbol(s)) = self.content {
      return s;
    }
    panic!("expected symbol")
  }

  pub fn as_node_literal(&self) -> Node {
    if let LiteralVal(Val::Node(n)) = self.content {
      return n;
    }
    panic!("expected node")
  }

  pub fn as_operator_literal(&self) -> Operator {
    if let LiteralVal(Val::Operator(op)) = self.content {
      return op;
    }
    panic!("expected operator")
  }
}

pub enum ExprShape<'l> {
  List(ExprTag, &'l [Expr]),
  Literal(Val),
  Ref(Symbol),
}

use ExprTag::*;

struct ParseState {
  st : SymbolTable,
}

pub fn parse_to_expr(st : SymbolTable, root : Node) -> Expr {
  let mut ps = ParseState { st };
  parse_expr(&mut ps, root)
}

fn list_expr(n : Node, tag : ExprTag, exprs : &[Expr]) -> Expr {
  expr(n, tag, List(perm_slice(exprs)))
}

fn list_expr_from_vec(n : Node, tag : ExprTag, exprs : Vec<Expr>) -> Expr {
  expr(n, tag, List(perm_slice_from_vec(exprs)))
}

fn literal_expr(n : Node, v : Val) -> Expr {
  expr(n, LiteralExpr, LiteralVal(v))
}

fn symbol_literal(n : Node) -> Expr {
  literal_expr(n, Val::Symbol(n.as_symbol()))
}

fn expr(n : Node, tag : ExprTag, content: ExprContent) -> Expr {
  let ed = ExprData {
    tag,
    content,
    loc: n.loc,
  };
  perm(ed)
}

fn parse_expr(ps : &mut ParseState, n : Node) -> Expr {
  match sexp::node_shape(&n) {
    // symbol reference (local var or global def)
    Atom(s) => {
      match s {
        "return" => list_expr(n, Return, &[]),
        "break" => list_expr(n, Break, &[]),
        "repeat" => list_expr(n, Repeat, &[]),
        "true" => literal_expr(n, Val::Bool(true)),
        "false" => literal_expr(n, Val::Bool(true)),
        _ => {
          let sym = n.as_symbol();
          expr(n, Reference, LiteralVal(Val::Symbol(sym)))
        }
      }
    }
    // literal
    Literal(l) => {
      match l {
        NodeLiteral::U64(v) => {
          literal_expr(n, Val::U64(v))
        }
        NodeLiteral::String(s) => {
          literal_expr(n, Val::String(s))
        }
      }
    }
    // break to label
    Command("break", [label]) => {
      let label_expr = symbol_literal(*label);
      list_expr(n, Break, &[label_expr])
    }
    // repeat to label
    Command("repeat", [label]) => {
      let label_expr = symbol_literal(*label);
      list_expr(n, Repeat, &[label_expr])
    }
    // array
    Command("array", elements) => {
      let es = parse_expr_list(ps, elements);
      expr(n, ArrayInit, List(es))
    }
    // array length
    Command("array_len", [e]) => {
      list_expr(n, ArrayLen, &[parse_expr(ps, *e)])
    }
    // array to slice
    Command("as_slice", [e]) => {
      list_expr(n, ArrayAsSlice, &[parse_expr(ps, *e)])
    }
    // array index
    Command("index", [v, index]) => {
      list_expr(n, ArrayIndex, &[
        parse_expr(ps, *v), // array
        parse_expr(ps, *index), // index
      ])
    }
    // ptr index
    Command("ptr_index", [v, index]) => {
      list_expr(n, PtrIndex, &[
        parse_expr(ps, *v), // pointer
        parse_expr(ps, *index), // index
      ])
    }
    // slice type
    Command("slice_index", [v, index]) => {
      let nb = NodeBuilder { loc: n.loc, st: ps.st };
      let slice_node = node_macros::slice_index_macro(&nb, *v, *index);
      parse_expr(ps, slice_node)
    }
    // init
    Command("zero_init", [type_node]) => {
      list_expr(n, ZeroInit, &[parse_expr(ps, *type_node)])
    }
    // init
    Command("init", ns) => {
      let type_value = parse_expr(ps, ns[0]);
      let mut v = vec![type_value];
      parse_to_vec(ps, &ns[1..], &mut v);
      list_expr_from_vec(n, StructInit, v)
    }
    // field deref
    Command(".", [v, field]) => {
      let struct_val = parse_expr(ps, *v);
      let field_name = symbol_literal(*field);
      list_expr(n, FieldIndex, &[struct_val, field_name])
    }
    // template
    Command("#", [quoted]) => {
      to_template_expr(ps, n, *quoted)
    }
    // for
    Command("for", [loop_var, start, end, body]) => {
      let nb = NodeBuilder { loc: n.loc, st: ps.st };
      let for_node = node_macros::for_macro(&nb, *loop_var, *start, *end, *body);
      parse_expr(ps, for_node)
    }
    // while
    Command("while", [cond, body]) => {
      let nb = NodeBuilder { loc: n.loc, st: ps.st };
      let while_node = node_macros::while_macro(&nb, *cond, *body);
      parse_expr(ps, while_node)
    }
    // quotation
    Command("quote", [quoted]) => {
      literal_expr(n, Val::Node(*quoted))
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      let (args, body) = to_args_body(ps, *arg_nodes, *body);
      let ret = list_expr(n, Implicit, &[]);
      list_expr(n, Fun, &[args, ret, body])
    }
    Command("fun", [arg_nodes, return_tag, body]) => {
      let (args, body) = to_args_body(ps, *arg_nodes, *body);
      let ret = parse_expr(ps, *return_tag);
      list_expr(n, Fun, &[args, ret, body])
    }
    // set var
    Command("set", [dest, value]) => {
      let dest = parse_expr(ps, *dest);
      let value = parse_expr(ps, *value);
      list_expr(n, Set, &[dest, value])
    }
    // let
    Command("let", [var_name, value]) => {
      let name = symbol_literal(*var_name);
      let val_expr = parse_expr(ps, *value);
      list_expr(n, Let, &[name, val_expr])
    }
    // if then
    Command("if", [cond_node, then_expr]) => {
      list_expr(n, IfElse, &[
        parse_expr(ps, *cond_node),
        parse_expr(ps, *then_expr),
      ])
    }
    // if then else
    Command("if", [cond_node, then_expr, else_expr]) => {
      list_expr(n, IfElse, &[
        parse_expr(ps, *cond_node),
        parse_expr(ps, *then_expr),
        parse_expr(ps, *else_expr),
      ])
    }
    // label expression
    Command("label", [label, body]) => {
      list_expr(n, LabelledBlock, &[
        symbol_literal(*label),
        parse_expr(ps, *body),
      ])
    }
    Command("do", exprs) => {
      let es = parse_expr_list(ps, exprs);
      expr(n, Do, List(es))
    }
  // Debug
    Command("debug", [v]) => {
      list_expr(n, Debug, &[parse_expr(ps, *v)])
    }
    // Return
    Command("return", [v]) => {
      list_expr(n, Return, &[parse_expr(ps, *v)])
    }
    // symbol
    Command("sym", [v]) => {
      symbol_literal(*v)
    }
    // typeof
    Command("typeof", [v]) => {
      list_expr(n, TypeOf, &[parse_expr(ps, *v)])
    }
    // load
    Command("*", [pointer]) => {
      list_expr(n, Deref, &[parse_expr(ps, *pointer)])
    }
    // ref
    Command("ref", [locator]) => {
      list_expr(n, GetAddress, &[parse_expr(ps, *locator)])
    }
    // cast
    Command("cast", [value, type_tag]) => {
      list_expr(n, Cast, &[
        parse_expr(ps, *value),
        parse_expr(ps, *type_tag),
      ])
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      list_expr(n, PtrType, &[parse_expr(ps, *inner_type)])
    }
    // function type
    Command("fn", [args, ret]) => {
      list_expr(n, FnType, &[
        to_type_args_list(ps, *args),
        parse_expr(ps, *ret),
      ])
    }
    // c function type
    Command("cfun", [args, ret]) => {
      list_expr(n, CFunType, &[
        to_type_args_list(ps, *args),
        parse_expr(ps, *ret),
      ])
    }
    // array type
    Command("sized_array", [element, length]) => {
      list_expr(n, SizedArrayType, &[
        parse_expr(ps, *element),
        parse_expr(ps, *length),
      ])
    }
    // struct type
    Command("struct", fields) => {
      let fs = to_field_list(ps, fields);
      expr(n, StructType, List(fs))
    }
    // slice type
    Command("slice", [element_type]) => {
      let nb = NodeBuilder { loc: n.loc, st: ps.st };
      let slice_node = node_macros::slice_type_macro(&nb, *element_type);
      parse_expr(ps, slice_node)
    }
    _ => {
      let ns = n.children();
      if ns.len() == 0 {
        return literal_expr(n, Val::Void);
      }
      if let [lit, tag] = ns {
        if let NodeContent::Literal(NodeLiteral::U64(_)) = lit.content {
          return list_expr(n, Cast, &[
            parse_expr(ps, *lit),
            parse_expr(ps, *tag),
          ]);
        }
      }
      if let NodeContent::Sym(sym) = ns[0].content {
        if let Some(op) = str_to_operator(sym.as_str()) {
          let op = literal_expr(ns[0], Val::Operator(op));
          let mut es = vec![op];
          parse_to_vec(ps, &ns[1..], &mut es);
          return list_expr_from_vec(n, InstrinicOp, es);
        }
      }
      let es = parse_expr_list(ps, ns);
      expr(n, Call, List(es))
    }
  }
}

fn to_args_body(ps : &mut ParseState, args_node : Node, body : Node) -> (Expr, Expr) {
  let mut v = Vec::with_capacity(args_node.children().len());
  for &a in args_node.children() {
    if let [name, type_tag] = a.children() {
      let name_literal = symbol_literal(*name);
      let type_expr = parse_expr(ps, *type_tag);
      let arg = list_expr(a, Syntax, &[name_literal, type_expr]);
      v.push(arg);
    }
    else {
      panic!()
    }
  }
  let args = perm_slice_from_vec(v);
  let body = parse_expr(ps, body);
  let args_expr = expr(args_node, Syntax, List(args));
  (args_expr, body)
}

fn to_type_args_list(ps : &mut ParseState, args : Node) -> Expr {
  let mut v = Vec::with_capacity(args.children().len());
  for a in args.children() {
    let arg = {
      if let [type_tag] = a.children() {
        parse_expr(ps, *type_tag)
      }
      else if let [name, type_tag] = a.children() {
        name.as_symbol();
        parse_expr(ps, *type_tag)
      }
      else {
        panic!("expected arg at ({})", a.loc)
      }
    };
    v.push(arg);
  }
  expr(args, Syntax, List(perm_slice_from_vec(v)))
}

fn to_field_list(ps : &mut ParseState, fields : &[Node]) -> SlicePtr<Expr> {
  let mut v = Vec::with_capacity(fields.len());
  for f in fields {
    let field = {
      if let [name, type_tag] = f.children() {
        list_expr(*f, Syntax, &[
          symbol_literal(*name),
          parse_expr(ps, *type_tag),
        ])
      }
      else {
        panic!("expected field at ({})", f.loc)
      }
    };
    v.push(field);
  }
  perm_slice_from_vec(v)
}

fn parse_expr_list(ps : &mut ParseState, ns : &[Node]) -> SlicePtr<Expr> {
  let mut v = Vec::with_capacity(ns.len());
  parse_to_vec(ps, ns, &mut v);
  perm_slice_from_vec(v)
}

fn parse_to_vec(ps : &mut ParseState, ns : &[Node], v : &mut Vec<Expr>) {
  for &n in ns {
    v.push(parse_expr(ps, n));
  }
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
    "!=" => NEq,
    "<" => LT,
    ">" => GT,
    "<=" => LTE,
    ">=" => GTE,
    "!" => Not,
    _ => return None,
  };
  Some(op)
}

fn to_template_expr(ps : &mut ParseState, n : Node, quoted : Node) -> Expr {
  
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
    let nb = NodeBuilder { loc: quoted.loc, st: ps.st };
    let n = template_macro(&nb, quoted, template_args.as_slice());
    parse_expr(ps, n)
  }
  else {
    literal_expr(n, Val::Node(quoted))
  }
}


use crate::{sexp, symbols};
use sexp::{
  Node,
  NodeLiteral,
  NodeShape::*,
};
use symbols::Symbol;

pub struct TaggedNode {
  pub n : Node,
  pub tag : ExprTag,
}


#[derive(Copy, Clone)]
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
  Operator,
  Call,
  Cast,
  IfElse,
  Debug,
  Set,
  Return,
  Break,
  Repeat,
  Label,
  Do,
  Template,
  Quote,
  Fun,
  Macro,
}

use ExprTag::*;

pub fn tag_nodes(root : Node) -> Vec<TaggedNode> {
  let mut ts = TagState {
    expr_tags: vec![],
    locals: vec![],
  };
  tag_node(&mut ts, root);
  return ts.expr_tags;
}

struct TagState {
  expr_tags : Vec<TaggedNode>,
  locals : Vec<Symbol>,
}

impl TagState {
  fn tag(&mut self, n : Node, tag : ExprTag) {
    self.expr_tags.push(TaggedNode{ n, tag });
  }
}

fn tag_node(ts : &mut TagState, n : Node) {
  match sexp::node_shape(&n) {
    // return
    Atom("return") => {
      ts.tag(n, Return);
    }
    // break
    Atom("break") => {
      ts.tag(n, Break);
    }
    // repeat
    Atom("repeat") => {
      ts.tag(n, Repeat);
    }
    // true
    Atom("true") => {
      ts.tag(n, LiteralBool);
    }
    // false
    Atom("false") => {
      ts.tag(n, LiteralBool);
    }
    // symbol reference (local var or global def)
    Atom(s) => {
      let tag = match s {
        "return" => Return,
        "break" => Break,
        "repeat" => Repeat,
        "true" | "false" => LiteralBool,
        _ => {
          if ts.locals.contains(&n.as_symbol()) {
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
    Command("break", [label]) => {
      ts.tag(n, Break);
    }
    // repeat to label
    Command("repeat", [label]) => {
      ts.tag(n, Repeat);
    }
    // array
    Command("array", elements) => {
      ts.tag(n, ArrayInit);
    }
    // array length
    Command("array_len", [e]) => {
      ts.tag(n, Call);
    }
    // array index
    Command("index", [v, index]) => {
      ts.tag(n, Index);
    }
    // init
    Command("zero_init", [type_node]) => {
      ts.tag(n, ZeroInit);
    }
    // init
    Command("init", ns) => {
      ts.tag(n, StructInit);
    }
    // field deref
    Command(".", [v, field]) => {
      ts.tag(n, FieldIndex);
    }
    // template
    Command("#", [quoted]) => {
      ts.tag(n, Template);
    }
    // quotation
    Command("quote", [quoted]) => {
      ts.tag(n, Quote);
    }
    // def
    Command("def", [def_name, value]) => {
      ts.tag(n, Def);
    }
    // fun
    Command("fun", [arg_nodes, body]) => {
      ts.tag(n, Fun);
    }
    Command("fun", [arg_nodes, return_tag, body]) => {
      ts.tag(n, Fun);
    }
    // macro
    Command("macro", [arg, body]) => {
      ts.tag(n, Macro);
    }
    // set var
    Command("set", [dest, value]) => {
      
    }
    // let
    Command("let", [var_name, value]) => {
      
    }
    // if then
    Command("if", [cond_node, then_expr]) => {
      
    }
    // if then else
    Command("if", [cond, then_expr, else_expr]) => {
      
    }
    // label expression
    Command("label", [label, body]) => {
      
    }
    Command("do", exprs) => {
      
    }
  // Debug
    Command("debug", [n]) => {
      
    }
    // Return
    Command("return", [n]) => {
      
    }
    // symbol
    Command("sym", [n]) => {
      
    }
    // typeof
    Command("typeof", [n]) => {
      
    }
    // load
    Command("*", [pointer]) => {
      
    }
    // ref
    Command("ref", [locator]) => {
      
    }
    // cast
    Command("cast", [value, type_tag]) => {
      
    }
    // pointer type
    Command("ptr", [inner_type]) => {
      ts.tag(n, Type);
    }
    // function type
    Command("fn", [arg_nodes, ret]) => {
      
    }
    // c function type
    Command("cfun", [arg_nodes, ret]) => {
      
    }
    // array type
    Command("sized_array", [element, size]) => {
      
    }
    // struct type
    Command("struct", fields) => {
      
    }
    _ => {
      if let Some(type_tag) = try_node_to_type(b, node) {
        
      }
      else {
        compile_list_expr(b, node)
      }
    }
  }
}

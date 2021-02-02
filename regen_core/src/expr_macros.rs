
use crate::{parse::{self, Expr, ExprShape}, perm_alloc, sexp, symbols};
use parse::{ExprContent, ExprData};
use perm_alloc::{perm, perm_slice, perm_slice_from_vec};
use symbols::{SymbolTable, to_symbol};
use sexp::{SrcLocation, Node, NodeContent, NodeInfo};

pub fn template(e : Expr, args : &[Expr]) -> Expr {
  pub fn template_inner(e : Expr, args : &[Expr], next_arg : &mut usize) -> Expr {
    use ExprShape::*;
    match e.shape() {
      Ref(_) | Literal(_) => e,
      List(TemplateHole, &[_]) => {
        let new_e = args[*next_arg];
        *next_arg += 1;
        new_e
      }
      List(_, cs) => {
        let mut children = vec![];
        for &c in cs {
          children.push(template_inner(c, args, next_arg));
        }
        let loc = e.loc;
        let ed = ExprData {
          tag: e.tag,
          content: ExprContent::List(perm_slice_from_vec(children)),
          loc: e.loc,
        };
        perm(ed)
      },
    }
  }
  template_inner(e, args, &mut 0)
}

pub struct NodeBuilder {
  pub loc : SrcLocation,
  pub st : SymbolTable,
}
  
impl NodeBuilder {
  fn list(&self, ns : &[Node]) -> Node {
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::List(perm_slice(ns)),
    })
  }

  fn list_from_vec(&self, ns : Vec<Node>) -> Node {
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::List(perm_slice_from_vec(ns)),
    })
  }

  fn sexp(&self, s : &str) -> Expr {
    parse::parse_expression(self.st, &self.loc.module.name, s)
  }

  fn sym(&self, s : &str) -> Node {
    let sym = to_symbol(self.st, s);
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::Sym(sym),
    })
  }
}

pub fn template_macro(nb : &NodeBuilder, n : Node, args : &[Node]) -> Node {
  // (do
  //   (let args (array a b c))
  //   (template_quote n (ref args) (array_len args))
  // )
  let mut array = vec![nb.sym("array")];
  array.extend_from_slice(args);
  nb.list(&[
    nb.sym("do"),
    nb.list(&[nb.sym("let"), nb.sym("args"), nb.list_from_vec(array)]),
    nb.list(&[
      nb.sym("template_quote"),
        nb.list(&[nb.sym("quote"), n]),
        nb.list(&[nb.sym("cast"),
          nb.list(&[nb.sym("ref"), nb.sym("args")]),
          nb.list(&[nb.sym("ptr"), nb.sym("node")]),
        ]),
        nb.list(&[nb.sym("array_len"), nb.sym("args")]),
    ])
  ])
}

pub fn for_macro(nb : &NodeBuilder, loop_var : Expr, start : Expr, end : Expr, body : Expr) -> Expr {
  let loop_template = nb.sexp("
    (do
      (let ($ loop_var) ($ start))
      (let _end ($ end))
      (label for_loop (do
        (if (< ($ loop_var) _end) (do
          ($ body)
          (set ($ loop_var) (+ ($ loop_var) 1))
          repeat
        ))
      ))
    )
  ");
  template(loop_template, &[loop_var, start, end, loop_var, body, loop_var, loop_var])
}

pub fn while_macro(nb : &NodeBuilder, cond : Expr, body : Expr) -> Expr {
  let loop_template = nb.sexp("
    (label while_loop (do
      (if ($ cond) (do
        ($ body)
        repeat
      ))
    ))
  ");
  template(loop_template, &[cond, body])
}

pub fn slice_index_macro(nb : &NodeBuilder, slice : Expr, index : Expr) -> Expr {
  let slice_index_template =
    nb.sexp("(ptr_index (. ($ slice) data) ($ index))");
  template(slice_index_template, &[slice, index])
}

pub fn slice_type_macro(nb : &NodeBuilder, element_type : Expr) -> Expr {
  let slice_type_template =
    nb.sexp("(struct (data (ptr ($ element_type)) (len u64))");
  template(slice_type_template, &[element_type])
}

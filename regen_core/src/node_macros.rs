
use crate::{perm_alloc, symbols, sexp};
use perm_alloc::{perm, perm_slice, perm_slice_from_vec};
use symbols::{SymbolTable, to_symbol};
use sexp::{SrcLocation, Node, NodeContent, NodeInfo, node_shape, NodeShape};

pub fn template(n : Node, args : &[Node]) -> Node {
  pub fn template_inner(n : Node, args : &[Node], next_arg : &mut usize) -> Node {
    use NodeShape::*;
    match node_shape(&n) {
      Atom(_) | Literal(_) => n,
      Command("$", [_]) => {
        let new_e = args[*next_arg];
        *next_arg += 1;
        new_e
      }
      _ => {
        let mut children = vec![];
        for &c in n.children() {
          children.push(template_inner(c, args, next_arg));
        }
        let loc = n.loc;
        let content = NodeContent::List(perm_slice_from_vec(children));
        perm(NodeInfo { loc, content })
      },
    }
  }
  template_inner(n, args, &mut 0)
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

  fn sexp(&self, s : &str) -> Node {
    sexp::sexp_list(self.st, &self.loc.module.name, s).children()[0]
  }

  fn sym(&self, s : &str) -> Node {
    let sym = to_symbol(self.st, s);
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::Sym(sym),
    })
  }
}

pub fn def_macro(nb : &NodeBuilder, def_name : Node, value : Node) -> Node {
  // (do ... )
  nb.list(&[
    nb.sym("do"),
    // (let v ($ value))
    nb.list(&[nb.sym("let"), nb.sym("v"), value]),
    // (let t (typeof v))
    nb.list(&[nb.sym("let"), nb.sym("t"),
      nb.list(&[nb.sym("typeof"), nb.sym("v")]),
    ]),
    // (let p (env_alloc_global env (sym ($ def_name)) t))
    nb.list(&[nb.sym("let"), nb.sym("p"),
      nb.list(&[
        nb.sym("env_alloc_global"),
        nb.sym("env"),
        nb.list(&[nb.sym("sym"), def_name]),
        nb.sym("t"),
      ]),    
    ]),
    // (memcpy p (cast (ref v) (ptr void)) (type_sizeof t))
    nb.list(&[nb.sym("memcpy"), nb.sym("p"),
      nb.list(&[nb.sym("cast"),
        nb.list(&[nb.sym("ref"), nb.sym("v")]),
        nb.list(&[nb.sym("ptr"), nb.sym("void")]),
      ]),
      nb.list(&[nb.sym("type_sizeof"), nb.sym("t")]),
    ]),
  ])
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

pub fn for_macro(nb : &NodeBuilder, loop_var : Node, start : Node, end : Node, body : Node) -> Node {
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

pub fn while_macro(nb : &NodeBuilder, cond : Node, body : Node) -> Node {
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

pub fn slice_index_macro(nb : &NodeBuilder, slice : Node, index : Node) -> Node {
  // (ptr_index (. slice data) index)
  nb.list(
    &[nb.sym("ptr_index"),
      nb.list(&[nb.sym("."), slice, nb.sym("data")]),
      index,
  ])
}

pub fn slice_type_macro(nb : &NodeBuilder, element_type : Node) -> Node {
  nb.list(
    &[nb.sym("struct"),
      nb.list(&[
        nb.sym("data"),
        nb.list(&[nb.sym("ptr"), element_type]),
      ]),
      nb.list(&[nb.sym("len"), nb.sym("u64")]),
  ])
}

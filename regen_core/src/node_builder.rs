
use crate::{
  perm_alloc::{perm, perm_slice, perm_slice_from_vec},
  symbols::{SymbolTable, to_symbol},
  parse::{SrcLocation, Node, NodeContent, NodeInfo}
};

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

  fn sym(&self, s : &str) -> Node {
    let sym = to_symbol(self.st, s);
    perm(NodeInfo {
      loc: self.loc,
      content: NodeContent::Sym(sym),
    })
  }
}

// impl ListToNode for NodeBuilder

pub fn def_macro(nb : &NodeBuilder, def_name : Node, value : Node) -> Node {
  nb.list(&[
    nb.sym("do"),
    nb.list(&[nb.sym("let"), nb.sym("v"), value]),
    nb.list(&[
      nb.sym("env_insert"),
      nb.sym("env"),
      nb.list(&[nb.sym("sym"), def_name]),
      nb.sym("v"),
      nb.list(&[nb.sym("typeof"), nb.sym("v")])])
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
        nb.list(&[nb.sym("ref"), nb.sym("args")]),
        nb.list(&[nb.sym("array_len"), nb.sym("args")]),
    ])
  ])
}

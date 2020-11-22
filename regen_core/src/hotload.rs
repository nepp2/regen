
use crate::{
  parse,
  parse::{
    Node, NodeShape, NodeContent, NodeLiteral::*,
  },
  symbols::Symbol,
  env::Env,
  interpret,
};

use std::collections::HashMap;

pub struct HotloadState {
  defs : HashMap<Symbol, Node>,
}

impl HotloadState {
  pub fn new() -> Self {
    HotloadState { defs: HashMap::new() }
  }
}

fn node_content_eq(a : Node, b : Node) -> bool {
  use NodeContent::*;
  match (a.content, b.content) {
    (Sym(a), Sym(b)) => a == b,
    (Literal(U64(a)), Literal(U64(b))) => a == b,
    (List(a_children), List(b_children)) => {
      a_children.as_slice().iter()
        .zip(b_children.as_slice().iter())
        .all(|(a, b)| node_content_eq(*a, *b))
    }
    _ => false,
  }
}

pub fn hotload_changes(code : &str, mut env : Env, hs : &mut HotloadState) {
  let n = parse::parse(env.st, &code);
  for c in n.children() {
    match parse::node_shape(c) {
      NodeShape::Command("def", [name, expr]) => {
        let name = name.as_symbol();
        if let Some(prev_expr) = hs.defs.get(&name) {
          if node_content_eq(*expr, *prev_expr) {
            continue;
          }
          else {
            env.values.remove(&name);
          }
        }
        println!("Loading def {}", name);
        hs.defs.insert(name, *expr);
      }
      _ => (),
    }
    interpret::interpret_node(*c, env);
  }
}

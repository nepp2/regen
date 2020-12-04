
use crate::{
  env::Env,
  sexp,
  sexp::{
    Node, NodeShape, NodeContent, NodeLiteral::*,
  },
  parse,
  symbols::{Symbol, to_symbol},
  interpret,
};

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::fmt::Write;
use crc32fast;

type SymbolGraph = HashMap<Symbol, HashSet<Symbol>>;

pub struct HotloadState {
  defs : Vec<(Symbol, Def)>,
  graph : SymbolGraph,
}

impl HotloadState {
  pub fn new() -> Self {
    HotloadState {
      defs: vec![],
      graph: HashMap::new(),
    }
  }
}

#[derive(Clone, Copy)]
struct Def {
  def_node : Node,
  value_expr : ComparableNode,
}

fn def(def_node : Node, value_expr : ComparableNode) -> Def {
  Def { def_node, value_expr }
}

/// Allows nodes to be compared based on their content.
/// Comparisons are not influenced by source locations.
#[derive(Clone, Copy)]
struct ComparableNode {
  n : Node,
  checksum : u32,
}

impl ComparableNode {
  fn new(n : Node) -> Self {
    let mut hasher = crc32fast::Hasher::new();
    node_content_hash(n, &mut hasher);
    let checksum = hasher.finalize();
    ComparableNode { n, checksum }
  }
}

impl PartialEq for ComparableNode {
  fn eq(&self, other: &Self) -> bool {
    self.checksum == other.checksum
      && node_content_eq(self.n, other.n)
  }
}
impl Eq for ComparableNode {}

impl Hash for ComparableNode {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.checksum.hash(state);
  }
}

fn node_content_hash<H: Hasher>(n : Node, state: &mut H) {
  use NodeContent::*;
  match n.content {
    Sym(s) => s.hash(state),
    Literal(U64(v)) => v.hash(state),
    Literal(String(s)) => s.as_str().hash(state),
    List(children) => {
      for &c in children {
        node_content_hash(c, state);
      }
    }
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

fn mark_dirty_defs(hs : &HotloadState, clean_defs : &mut HashSet<Symbol>, dirty_name : Symbol) {
  for (&name, dependencies) in &hs.graph {
    if name != dirty_name && dependencies.contains(&dirty_name) {
      if clean_defs.remove(&name) {
        mark_dirty_defs(hs, clean_defs, name);
      }
    }
  }
}

pub fn clear_state(mut env : Env, hs : &mut HotloadState) {
  hs.graph.clear();
  for (name, _) in hs.defs.drain(..) {
    env.values.remove(&name); // TODO: leaks memory
  }
}

pub fn hotload_changes(code : &str, mut env : Env, hs : &mut HotloadState) {
  let n = sexp::sexp_list(env.st, &code);
  let old_defs : HashMap<_, _> =
    hs.defs.iter().enumerate()
    .map(|(i, (name, _))| (*name, i))
    .collect();
  let mut loaded_defs = vec![];
  let mut new_names = vec![];
  let mut unmodified_defs = HashSet::new();

  let mut symbol_buffer = "".to_string();
  let mut loaded_names = HashSet::new();

  // Find new and unchanged defs
  for c in n.children() {
    match sexp::node_shape(c) {
      NodeShape::Command("def", [name, expr]) => {
        let name = name.as_symbol();
        let expr = ComparableNode::new(*expr);
        loaded_defs.push((name, def(*c, expr)));
        loaded_names.insert(name);
      }
      _ => {
        let expr = ComparableNode::new(*c);
        let mut i = 0;
        loop {
          symbol_buffer.clear();
          write!(symbol_buffer, "__toplevel__{}_{}", expr.checksum, i).unwrap();
          let name = to_symbol(env.st, &symbol_buffer);
          if !loaded_names.contains(&name) {
            // Do stuff
            loaded_defs.push((name, def(*c, expr)));
            loaded_names.insert(name);
            break;
          }
          i += 1;
        }
      }
    }
  }
  for (name, def) in &loaded_defs {
    if let Some(&index) = old_defs.get(name) {
      let old_def = &hs.defs[index].1;
      if def.value_expr == old_def.value_expr {
        unmodified_defs.insert(*name);
      }
    }
    else {
      new_names.push(*name);
    }
  }

  // recursively mark modified defs
  for (name, _) in &hs.defs {
    if !unmodified_defs.contains(name) {
      mark_dirty_defs(hs, &mut unmodified_defs, *name);
    }
  }
  for name in new_names {
    // might have introduced a symbol that an existing symbol tries to reference
    mark_dirty_defs(hs, &mut unmodified_defs, name);
  }
  // remove any modified def (changed or deleted)
  for (name, _) in &hs.defs {
    if !unmodified_defs.contains(name) {
      // println!("unload def '{}'", name);
      hs.graph.remove(&name);
      env.values.remove(name); // TODO: leaks memory
    }
  }
  // load defs
  for (name, def) in &loaded_defs {
    if !unmodified_defs.contains(name) {
      // println!("loading def '{}'", name);
      let (expr, global_references) =
      parse::parse_to_expr_with_global_references(env.st, def.def_node);
      hs.graph.insert(*name, global_references);
      interpret::interpret_expr(expr, env);
    }
  }
  hs.defs = loaded_defs;
}


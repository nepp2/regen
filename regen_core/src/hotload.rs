
use crate::{
  env::Env,
  sexp,
  sexp::{
    Node, NodeShape, NodeContent, NodeLiteral::*,
  },
  parse,
  symbols::Symbol,
  interpret,
  graph,
};

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use crc32fast;

type SymbolGraph = HashMap<Symbol, HashSet<Symbol>>;

pub struct HotloadState {
  defs : HashMap<Symbol, Def>,
  graph : SymbolGraph,
}

impl HotloadState {
  pub fn new() -> Self {
    HotloadState {
      defs: HashMap::new(),
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

fn find_dependent_defs(hs : &HotloadState, name : Symbol, dependents : &mut HashSet<Symbol>) {
  for (&def, dependencies) in &hs.graph {
    if def != name && dependencies.contains(&name) {
      if !dependents.contains(&def) {
        dependents.insert(def);
        find_dependent_defs(hs, def, dependents);
      }
    }
  }
}

pub fn hotload_changes(code : &str, mut env : Env, hs : &mut HotloadState) {
  let n = sexp::sexp_list(env.st, &code);
  let mut new_defs = HashMap::new();
  let mut all_defs = HashSet::new();
  let mut modified_defs = vec![];
  // Find new and unchanged defs
  for c in n.children() {
    match sexp::node_shape(c) {
      NodeShape::Command("def", [name, expr]) => {
        let name = name.as_symbol();
        all_defs.insert(name);
        let expr = ComparableNode::new(*expr);
        if let Some(old_def) = hs.defs.get(&name) {
          if expr == old_def.value_expr {
            // unmodified def; do nothing
          }
          else {
            // modified def
            modified_defs.push((name, def(*c, expr)));
          }
        }
        else {
          // new def
          new_defs.insert(name, def(*c, expr));
        }
      }
      _ => (),
    }
  }
  // find deleted defs
  let mut deleted_defs = HashSet::new();
  for name in hs.defs.keys() {
    if !all_defs.contains(name) {
      deleted_defs.insert(*name);
    }
  }
  // dirty list contains deleted and modified defs, and their dependents
  let mut dirty_defs = HashSet::new();
  for name in &deleted_defs {
    dirty_defs.insert(*name);
    find_dependent_defs(&hs, *name, &mut dirty_defs);
  }
  for (name, _) in &modified_defs {
    dirty_defs.insert(*name);
    find_dependent_defs(&hs, *name, &mut dirty_defs);
  }
  for (name, _) in &new_defs {
    find_dependent_defs(&hs, *name, &mut dirty_defs);
  }
  // Find all defs that need to be loaded
  let mut build_set = HashMap::new();
  build_set.extend(new_defs.into_iter());
  build_set.extend(modified_defs);
  for name in &dirty_defs {
    if !build_set.contains_key(name) {
      if !deleted_defs.contains(name) {
        build_set.insert(*name, hs.defs[name]);
      }
    }
  }
  // unload dirty defs
  for name in &dirty_defs {
    println!("unload def '{}'", name);
    hs.defs.remove(name);
    hs.graph.remove(&name);
    env.values.remove(name); // TODO: leaks memory
  }
  // load defs
  for (&name, &def) in &build_set {
    println!("loading def '{}'", name);
    hs.defs.insert(name, def);
    let (expr, global_references) =
      parse::parse_to_expr_with_global_references(env.st, def.def_node);
    hs.graph.insert(name, global_references);

    // TODO: this happens in the wrong order!
    // interpret::interpret_expr(expr, env);
  }
}

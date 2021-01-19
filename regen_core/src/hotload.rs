
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
  defs : HashMap<Symbol, Def>,
  dependencies : SymbolGraph,
}

impl HotloadState {
  pub fn new() -> Self {
    HotloadState {
      defs: HashMap::new(),
      dependencies: HashMap::new(),
    }
  }
}

#[derive(Clone, Copy)]
struct Def {
  def_node : Node,
  value_expr : ComparableNode,
}

#[derive(Clone, Copy, PartialEq)]
enum DefState {
  Changed, Unchanged, Broken,
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
    (Literal(String(a)), Literal(String(b))) => a.as_str() == b.as_str(),
    (List(a_children), List(b_children)) => {
      a_children.as_slice().iter()
        .zip(b_children.as_slice().iter())
        .all(|(a, b)| node_content_eq(*a, *b))
    }
    _ => false,
  }
}

pub fn clear_state(mut env : Env, hs : &mut HotloadState) {
  hs.dependencies.clear();
  for (name, _) in hs.defs.drain() {
    env.values.remove(&name); // TODO: leaks memory
  }
}

fn unload_def(hs : &mut HotloadState, mut env : Env, name : Symbol){
  // println!("unload def '{}'", name);
  hs.dependencies.remove(&name);
  hs.defs.remove(&name);
  env.values.remove(&name); // TODO: leaks memory
}

fn load_def(
  hs : &mut HotloadState,
  env : Env,
  new_defs : &HashMap<Symbol, DefState>,
  def_node : Node,
  value_expr : ComparableNode,
  name : Symbol
) -> Result<(), ()>
{
  // println!("loading def '{}'", name);
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let aaa = ();
  let global_references = std::panic::catch_unwind(|| {
    let (expr, global_references) =
      parse::parse_to_expr_with_global_references(env.st, def_node);
    // check dependencies are available
    for n in global_references.iter() {
      match new_defs.get(n) {
        Some(DefState::Broken) => {
          panic!("Def `{}` depends on broken def `{}`", name, n);
        }
        None => {
          if hs.defs.contains_key(n) {
            panic!("Def `{}` is missing dependency `{}`", name, n);
          }
        }
        _ => (),
      }
    }
    interpret::interpret_expr(expr, env);
    global_references
  }).map_err(|_| ())?;
  hs.defs.insert(name, def(def_node, value_expr));
  hs.dependencies.insert(name, global_references);
  Ok(())
}

fn is_external_dependency(hs : &HotloadState, env : Env, new_defs : &mut HashMap<Symbol, DefState>, name : Symbol) -> bool {
  if !new_defs.contains_key(&name) {
    if !hs.defs.contains_key(&name) {
      if env.values.contains_key(&name) {
        return true;
      }
    }
  }
  false
}

fn hotload_def(
  hs : &mut HotloadState,
  env : Env,
  name : Symbol,
  def_node : Node,
  value_expr : ComparableNode,
  new_defs : &mut HashMap<Symbol, DefState>,
)
{
  let mut new_def_state : DefState = (|| {
    // if already defined
    if let Some(def) = hs.defs.iter().find(|d| *d.0 == name) {
      // check if expression has changed
      let mut changed = def.1.value_expr != value_expr;
      // check dependencies
      for n in hs.dependencies[&name].iter() {
        if !is_external_dependency(hs, env, new_defs, *n) {
          if new_defs.get(n) != Some(&DefState::Unchanged) {
            changed = true;
            break;
          }
        }
      }
      if changed {
        unload_def(hs, env, name);
        DefState::Changed
      }
      else {
        DefState::Unchanged
      }
    }
    else {
      DefState::Changed
    }
  }) ();
  if new_def_state == DefState::Changed {
    let r = load_def(hs, env, new_defs, def_node, value_expr, name);
    if r.is_err() {
      new_def_state = DefState::Broken;
    }
  }
  if new_def_state == DefState::Unchanged {
    // Update nodes so that their text locations are correct
    hs.defs.insert(name, def(def_node, value_expr));
  }
  new_defs.insert(name, new_def_state);
}

pub fn hotload_changes(module_name : &str, code : &str, env : Env, hs : &mut HotloadState) {
  // Parse file
  let n = {
    let r = std::panic::catch_unwind(|| {
      sexp::sexp_list(env.st, module_name, &code)
    });
    if let Ok(n) = r { n } else { return }
  };

  // Find new and unchanged defs
  let mut symbol_buffer = "".to_string();
  let mut new_defs = HashMap::new();
  for node in n.children() {
    match sexp::node_shape(node) {
      NodeShape::Command("def", [name, expr]) => {
        let name = name.as_symbol();
        let value_expr = ComparableNode::new(*expr);
        hotload_def(hs, env, name, *node, value_expr, &mut new_defs);
      }
      _ => {
        let value_expr = ComparableNode::new(*node);
        let mut i = 0;
        loop {
          symbol_buffer.clear();
          write!(symbol_buffer, "__toplevel__{}_{}", value_expr.checksum, i).unwrap();
          let name = to_symbol(env.st, &symbol_buffer);
          if !new_defs.contains_key(&name) {
            hotload_def(hs, env, name, *node, value_expr, &mut new_defs);
            break;
          }
          i += 1;
        }
      }
    }
  }

  // unload any defs that were deleted or broken
  let deletion_list : Vec<_> =
    hs.defs.keys().filter(|def| match new_defs.get(def) {
      None | Some(DefState::Broken) => true,
      _ => false,
    })
    .cloned().collect();
  for def in deletion_list {
    unload_def(hs, env, def)
  }
}

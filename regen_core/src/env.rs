use crate::{compile::Function, event_loop::{self, EventLoop, TriggerId}, ffi_libs::*, parse::{self, CodeModule, Expr, ExprContent, ExprTag, SrcLocation, Val}, perm_alloc::{Ptr, SlicePtr, perm, perm_slice}, symbols::{Symbol, SymbolTable, to_symbol}, types::{TypeHandle, CoreTypes, core_types }};

use std::collections::{HashMap, HashSet};
use std::fmt;

/// A heap allocated Regen value
#[derive(Clone, Copy)]
pub struct RegenValue {
  pub ptr : *mut (),
  pub t : TypeHandle,
}

/// Environment for regen editing session
#[derive(Clone)]
pub struct Environment {
  pub live_exprs : Vec<Expr>,
  pub cells : HashMap<CellUid, CellValue>,
  pub graph : CellGraph,
  pub timers : HashMap<CellUid, TriggerId>,
  pub event_loop : Ptr<EventLoop>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  builtin_dummy_expr : Expr,
}

#[derive(Copy, Clone)]
pub struct ReactiveObserver {
  pub uid : CellUid,
  pub input : CellUid,
  pub update_handler : *const Function,
}

#[derive(Clone, Default)]
pub struct CellGraph {
  /// The Uids of symbol dependencies are stored alongside the expression
  /// the Uid was calculated from, because these expressions are sensitive
  /// to namespacing. When the code changes they have to be re-checked in
  /// case they resolve to a different Uid.
  dependency_graph : HashMap<CellUid, HashSet<CellUid>>,

  /// The output graph is just the dependency graph inverted
  output_graph : HashMap<CellUid, HashSet<CellUid>>,

  /// Maps from reactive cells to the cells they observe
  pub reactive_observers : HashMap<CellUid, ReactiveObserver>,
}

impl CellGraph {
  pub fn dependencies(&self, uid : CellUid) -> Option<&HashSet<CellUid>> {
    self.dependency_graph.get(&uid)
  }

  pub fn outputs(&self, uid : CellUid) -> Option<&HashSet<CellUid>> {
    self.output_graph.get(&uid)
  }

  fn clear_dependencies(&mut self, uid : CellUid) {
    if let Some(deps) = self.dependency_graph.remove(&uid) {
      for dep_uid in &deps {
        if let Some(outputs) = self.output_graph.get_mut(dep_uid) {
          outputs.remove(&uid);
        }
      }
    }
  }

  pub fn unload_cell(&mut self, uid : CellUid) {
    self.clear_dependencies(uid);
    self.reactive_observers.remove(&uid);
  }

  pub fn set_cell_dependencies(&mut self, uid : CellUid, deps : HashSet<CellUid>) {
    // make sure any old data is removed
    self.clear_dependencies(uid);
    // add the outputs
    for &dep_uid in &deps {
      let outputs = self.output_graph.entry(dep_uid).or_insert_with(|| HashSet::new());
      outputs.insert(uid);
    }
    self.dependency_graph.insert(uid, deps);
  }
}

pub type Env = Ptr<Environment>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CellId { 
  DefCell(Symbol),
  ExprCell(Expr),
}

pub type Namespace = SlicePtr<Symbol>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellUid {
  pub id : CellId,
  pub namespace : SlicePtr<Symbol>,
}
use CellId::*;

#[derive(Clone, Copy)]
pub struct CellValue {
  pub full_expr : Expr,
  pub value_expr : Expr,
  pub t : TypeHandle,
  pub ptr : *const (),
  pub has_value : bool,
}

pub fn new_namespace(names : &[Symbol]) -> Namespace {
  perm_slice(names)
}

impl CellUid {
  pub fn def(name : Symbol, namespace : Namespace) -> CellUid {
    CellUid { id: DefCell(name), namespace }
  }

  pub fn expr(e : Expr, namespace : Namespace) -> CellUid {
    CellUid { id: ExprCell(e), namespace }
  }
}

pub fn unload_cell(mut env : Env, uid : CellUid) {
  if let DefCell(_) = uid.id {
    let el = env.event_loop;
    if let Some(&id) = env.timers.get(&uid) {
      event_loop::remove_trigger(el, id);
    }
    env.timers.remove(&uid);
  }
  env.cells.remove(&uid);
  env.graph.unload_cell(uid);
}

pub fn get_cell_value(env : Env, uid : CellUid) -> Option<CellValue> {
  env.cells.get(&uid).cloned()
}

pub fn define_global(mut env : Env, s : &str, v : u64, t : TypeHandle) {
  let name = to_symbol(env.st, s);
  let path = CellUid::def(name, new_namespace(&[]));
  if env.cells.contains_key(&path) {
    panic!("def {} already defined", name);
  }
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  let e = env.builtin_dummy_expr;
  let cv = CellValue { full_expr: e, value_expr: e, t, ptr, has_value: true };
  env.cells.insert(path, cv);
  unsafe {
    *(ptr as *mut u64) = v;
  }
}

pub fn get_cell_type(e : Env, uid : CellUid) -> Option<TypeHandle> {
  e.cells.get(&uid).map(|entry| entry.t)
}

pub fn get_event_loop(e : Env) -> Ptr<EventLoop> {
  e.event_loop
}

pub fn new_env(st : SymbolTable) -> Env {
  let builtin_dummy_expr = {
    let module = perm(CodeModule {
      code: "".into(),
      name: to_symbol(st, "__internal"),
    });
    parse::expr(
      ExprTag::Omitted,
      ExprContent::Value(Val::Void),
      SrcLocation { start: 0, end: 0, module },
    )
  };

  let env = perm(Environment {
    live_exprs: vec![],
    cells: HashMap::new(),
    graph: Default::default(),
    timers: HashMap::new(),
    event_loop: event_loop::create_event_loop(),
    st,
    c: core_types(st),
    builtin_dummy_expr,
  });
  load_ffi_libs(env);
  env
}

impl fmt::Display for CellUid {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for n in self.namespace {
      write!(f, "{}::", n)?;
    }
    write!(f, "{}", self.id)
  }
}

impl fmt::Display for CellId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DefCell(name) => write!(f, "{}", name),
      ExprCell(expr) => write!(f, "expr({})", expr.loc()),
    }
  }
}

use crate::{event_loop::{self, EventLoop, Signal}, ffi_libs::*, parse::{self, CodeModule, Expr, ExprContent, ExprTag, SrcLocation, Val}, perm_alloc::{Ptr, SlicePtr, perm, perm_slice}, symbols::{Symbol, SymbolTable, to_symbol}, types::{TypeHandle, CoreTypes, core_types }};

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Environment for regen editing session
#[derive(Clone)]
pub struct Environment {
  pub live_exprs : Vec<Expr>,
  pub cells : HashMap<CellUid, CellValue>,
  pub graph : CellGraph,
  pub signals : HashMap<CellUid, Ptr<Signal>>,
  pub event_loop : Ptr<EventLoop>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  builtin_dummy_expr : Expr,
}

#[derive(Clone, Default)]
pub struct CellGraph {
  /// The Uids of symbol dependencies are stored alongside the expression
  /// the Uid was calculated from, because these expressions are sensitive
  /// to namespacing. When the code changes they have to be re-checked in
  /// case they resolve to a different Uid.
  dependency_graph : HashMap<CellUid, HashMap<CellUid, UidExpr>>,

  /// The cell observer graph is just the symbol dependency graph inverted
  observer_graph : HashMap<CellUid, HashSet<CellUid>>,
}

impl CellGraph {
  pub fn dependencies(&self, uid : CellUid) -> Option<&HashMap<CellUid, UidExpr>> {
    self.dependency_graph.get(&uid)
  }

  pub fn observers(&self, uid : CellUid) -> Option<&HashSet<CellUid>> {
    self.observer_graph.get(&uid)
  }

  pub fn unload_cell(&mut self, uid : CellUid) {
    if let Some(deps) = self.dependency_graph.remove(&uid) {
      for dep_uid in deps.keys() {
        if let Some(observers) = self.observer_graph.get_mut(dep_uid) {
          observers.remove(&uid);
        }
      }
    }
  }

  pub fn set_cell_dependencies(&mut self, uid : CellUid, deps : HashMap<CellUid, UidExpr>) {
    // make sure any old data is removed
    self.unload_cell(uid);
    // add the observers
    for &dep_uid in deps.keys() {
      let observers = self.observer_graph.entry(dep_uid).or_insert_with(|| HashSet::new());
      observers.insert(uid);
    }
    self.dependency_graph.insert(uid, deps);
  }
}

#[derive(Copy, Clone)]
// the expression that a CellUID was derived from
pub struct UidExpr(pub Expr);

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
    if let Some(&signal) = env.signals.get(&uid) {
      event_loop::remove_signal(el, signal);
    }
    env.signals.remove(&uid);
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
  env.cells.insert(path, CellValue { full_expr: e, value_expr: e, t, ptr });
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
    signals: HashMap::new(),
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

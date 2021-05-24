use crate::{codegen::Function, event_loop::{self, EventLoop, TimerId}, ffi_libs::*, parse::{self, CodeModule, Expr, ExprContent, ExprTag, SrcLocation, Val}, perm_alloc::{Ptr, SlicePtr, perm, perm_slice}, symbols::{Symbol, SymbolTable, to_symbol}, types::{TypeHandle, CoreTypes, core_types }};

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
  pub root_expr : Option<Expr>,

  /// TODO: these ids are not cleared, so they
  /// will leak memory over long editing sessions
  uid_counter : u64,
  cell_uids : HashMap<CellIdentifier, CellUid>,
  cell_ids : HashMap<CellUid, CellIdentifier>,

  pub active_cells : HashSet<CellUid>,
  pub update_lists : HashMap<CellUid, Vec<CellUid>>,

  pub cell_src : HashMap<CellUid, CellSrc>,
  pub cell_compiles : HashMap<CellUid, CellCompile>,
  pub cell_values : HashMap<CellUid, CellValue>,
  pub cell_build_status : HashMap<CellUid, BuildStatus>,
  pub broken_cells : HashSet<CellUid>,
  pub reactive_cells : HashMap<CellUid, ReactiveCell>,
  pub graph : CellGraph,

  pub watchers : HashMap<CellUid, RegenString>,
  pub timers : HashMap<CellUid, TimerId>,
  pub event_loop : Ptr<EventLoop>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  builtin_dummy_expr : Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuildStatus {
  Empty = 0,
  Compiled = 1,
  Evaluated = 2,
}

#[derive(Copy, Clone)]
pub struct CellCompile {
  pub reactive_constructor : Option<RegenValue>,
  pub allocation : RegenValue,
  pub function : Ptr<Function>,
}

#[derive(Copy, Clone, PartialEq)]
pub struct CellSrc {
  pub value_expr : Expr,
  pub is_reactive: bool,
}

#[derive(Copy, Clone)]
pub struct ReactiveCell {
  pub input : CellUid,
  pub update_handler : Ptr<Function>,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DependencyType {
  Code,
  Value,
  Observer,
}

#[derive(Clone, Default)]
pub struct CellGraph {
  /// The Uids of symbol dependencies are stored alongside the expression
  /// the Uid was calculated from, because these expressions are sensitive
  /// to namespacing. When the code changes they have to be re-checked in
  /// case they resolve to a different Uid.
  input_graph : HashMap<CellUid, HashMap<CellUid, DependencyType>>,

  /// The output graph is just the dependency graph inverted
  output_graph : HashMap<CellUid, HashMap<CellUid, DependencyType>>,
}

impl CellGraph {
  pub fn inputs(&self, uid : CellUid) -> Option<&HashMap<CellUid, DependencyType>> {
    self.input_graph.get(&uid)
  }

  pub fn outputs(&self, uid : CellUid) -> Option<&HashMap<CellUid, DependencyType>> {
    self.output_graph.get(&uid)
  }

  fn remove_uid(&mut self, uid : CellUid) {
    if let Some(deps) = self.input_graph.remove(&uid) {
      for input_uid in deps.keys() {
        if let Some(outputs) = self.output_graph.get_mut(input_uid) {
          outputs.remove(&uid);
        }
      }
    }
  }

  pub fn unload_cell(&mut self, uid : CellUid) {
    self.remove_uid(uid);
  }

  pub fn set_cell_dependencies(&mut self, uid : CellUid, deps : HashMap<CellUid, DependencyType>) {
    // make sure any old data is removed
    self.remove_uid(uid);
    // add the outputs
    for (&dep_uid, &dep_type) in &deps {
      let outputs =
        self.output_graph.entry(dep_uid).or_insert_with(|| HashMap::new());
      outputs.insert(uid, dep_type);
    }
    self.input_graph.insert(uid, deps);
  }
}

pub type Env = Ptr<Environment>;

pub type Namespace = SlicePtr<Symbol>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CellIdentifier {
  DefCell(Namespace, Symbol),
  ExprCell(Expr),
  EmbedCell(Expr),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct CellUid(u64);

impl CellUid {
  pub fn id(self, env : Env) -> CellIdentifier {
    env.cell_ids[&self]
  }

  pub fn loc(self, env : Env) -> SrcLocation {
    env.cell_src[&self].value_expr.loc()
  }
}

use CellIdentifier::*;

#[derive(Clone, Copy)]
pub struct CellValue {
  pub ptr : *mut (),
  pub t : TypeHandle,
  pub initialised : bool,
}

pub fn new_namespace(names : &[Symbol]) -> Namespace {
  perm_slice(names)
}

impl CellIdentifier {
  pub fn def(namespace : Namespace, name : Symbol) -> CellIdentifier {
    DefCell(namespace, name)
  }

  pub fn expr(e : Expr) -> CellIdentifier {
    ExprCell(e)
  }

  pub fn uid(self, mut env : Env) -> CellUid {
    if let Some(&uid) = env.cell_uids.get(&self) {
      uid
    }
    else {
      env.uid_counter += 1;
      let uid = CellUid(env.uid_counter);
      env.cell_uids.insert(self, uid);
      env.cell_ids.insert(uid, self);
      uid
    }
  }
}

pub fn get_cell_value(env : Env, uid : CellUid) -> Option<CellValue> {
  env.cell_values.get(&uid).cloned()
}

pub fn unload_cell(mut env : Env, uid : CellUid) {
  env.cell_src.remove(&uid);
  env.cell_build_status.remove(&uid);
  env.broken_cells.remove(&uid);
  env.graph.unload_cell(uid);
  unload_cell_compile(env, uid);
}

pub fn unload_cell_compile(mut env : Env, uid : CellUid) {
  env.cell_compiles.remove(&uid);
  unload_cell_value(env, uid);
}

pub fn unload_cell_value(mut env : Env, uid : CellUid) {
  if let Some(id) = env.timers.remove(&uid) {
    event_loop::remove_timer(env.event_loop, id);
  }
  if let Some(path) = env.watchers.remove(&uid) {
    event_loop::remove_watcher(env.event_loop, uid, path);
  }
  env.cell_values.remove(&uid);
  env.reactive_cells.remove(&uid);
}

pub fn define_global(mut env : Env, s : &str, v : u64, t : TypeHandle) {
  let name = to_symbol(env.st, s);
  let uid = CellIdentifier::def(new_namespace(&[]), name).uid(env);
  if env.cell_src.contains_key(&uid) {
    panic!("def {} already defined", name);
  }
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  let value_expr = env.builtin_dummy_expr;
  let cv = CellValue{ t, ptr, initialised: true };
  env.cell_src.insert(uid, CellSrc { value_expr, is_reactive: false });
  env.cell_values.insert(uid, cv);
  unsafe {
    *(ptr as *mut u64) = v;
  }
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
    root_expr: None,
    uid_counter: 0,
    cell_uids:  HashMap::new(),
    cell_ids:  HashMap::new(),
    active_cells: HashSet::new(),
    update_lists: HashMap::new(),
    cell_src: HashMap::new(),
    cell_compiles: HashMap::new(),
    cell_values: HashMap::new(),
    cell_build_status: HashMap::new(),
    broken_cells: HashSet::new(),
    reactive_cells: HashMap::new(),
    graph: Default::default(),
    timers: HashMap::new(),
    watchers: HashMap::new(),
    event_loop: event_loop::create_event_loop(),
    st,
    c: core_types(st),
    builtin_dummy_expr,
  });
  load_ffi_libs(env);
  env
}

impl fmt::Display for CellIdentifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DefCell(namespace, name) => {
        for n in namespace.as_slice() {
          write!(f, "{}::", n)?;
        }
        write!(f, "{}", name)
      },
      ExprCell(expr) => write!(f, "expr({})", expr.loc()),
      EmbedCell(expr) => write!(f, "embed({})", expr.loc()),
    }
  }
}

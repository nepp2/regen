use crate::{event_loop::{self, EventLoop, SignalId}, ffi_libs::*, parse::{self, CodeModule, Expr, ExprContent, ExprTag, SrcLocation, Val}, perm_alloc::{Ptr, SlicePtr, perm, perm_slice}, symbols::{Symbol, SymbolTable, to_symbol}, types::{TypeHandle, CoreTypes, core_types }};

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Environment for regen editing session
#[derive(Clone)]
pub struct Environment {
  pub cells : HashMap<CellUid, CellValue>,
  pub dependencies : HashMap<CellUid, HashSet<CellUid>>,
  signals : HashMap<CellUid, Vec<SignalId>>,
  pub event_loop : Ptr<EventLoop>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  pub active_definition : Option<CellUid>,
  builtin_dummy_expr : Expr,
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
  pub e : Expr,
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
    if let Some(signals) = env.signals.get(&uid) {
      for &id in signals {
        event_loop::destroy_signal(el, id);
      }
    }
    env.signals.remove(&uid);
  }
  env.cells.remove(&uid);
  env.dependencies.remove(&uid);
}

pub fn get_cell_value(env : Env, uid : CellUid) -> Option<CellValue> {
  env.cells.get(&uid).cloned()
}

fn env_alloc_global(mut env : Env, e : Expr, name : Symbol, t : TypeHandle) -> *mut () {
  let path = CellUid::def(name, new_namespace(&[]));
  if env.cells.contains_key(&path) {
    panic!("def {} already defined", name);
  }
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  env.cells.insert(path, CellValue { e, t, ptr });
  ptr
}

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let name = to_symbol(e.st, s);
  let p = env_alloc_global(e, e.builtin_dummy_expr, name, t);
  unsafe {
    *(p as *mut u64) = v;
  }
}

pub fn set_active_definition(mut env : Env, def : Option<CellUid>) {
  env.active_definition = def;
}

pub fn register_signal(mut env : Env, id : SignalId) {
  let name = env.active_definition.expect("can't register a signal; no active definition");
  let signals = env.signals.entry(name).or_insert_with(|| vec![]);
  signals.push(id);
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
      name: "__internal".into(),
    });
    parse::expr(
      ExprTag::Omitted,
      ExprContent::Value(Val::Void),
      SrcLocation { start: 0, end: 0, module },
    )
  };

  let env = perm(Environment {
    cells: HashMap::new(),
    dependencies: HashMap::new(),
    signals: HashMap::new(),
    event_loop: event_loop::create_event_loop(),
    st,
    c: core_types(st),
    active_definition: None,
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

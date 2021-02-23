use crate::{event_loop::{self, EventLoop, SignalId}, ffi_libs::*, parse::{self, CodeModule, Expr, ExprContent, ExprTag, SrcLocation, Val}, perm_alloc::{Ptr, perm}, symbols::{Symbol, SymbolTable, to_symbol}, types::{TypeHandle, CoreTypes, core_types }};

use std::collections::{HashMap, HashSet};
use std::fmt;

/// Environment for regen editing session
#[derive(Clone)]
pub struct Environment {
  pub cells : HashMap<CellId, CellValue>,
  pub dependencies : HashMap<CellId, HashSet<CellId>>,
  signals : HashMap<Symbol, Vec<SignalId>>,
  pub event_loop : Ptr<EventLoop>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  pub active_definition : Option<Symbol>,
  builtin_dummy_expr : Expr,
}

pub type Env = Ptr<Environment>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CellId { DefCell(Symbol), ConstCell(Expr) }

use CellId::*;

#[derive(Clone, Copy)]
pub struct CellValue {
  pub e : Expr,
  pub t : TypeHandle,
  pub ptr : *const (),
}

pub fn unload_cell(mut env : Env, id : CellId) {
  if let DefCell(name) = id {
    let el = env.event_loop;
    if let Some(signals) = env.signals.get(&name) {
      for &id in signals {
        event_loop::destroy_signal(el, id);
      }
    }
    env.signals.remove(&name);
  }
  env.cells.remove(&id);
  env.dependencies.remove(&id);
}

pub fn get_def_cell(env : &Env, name : Symbol) -> Option<&CellValue> {
  env.cells.get(&DefCell(name))
}

fn insert_cell(mut env : Env, e : Expr, id : CellId, t : TypeHandle, ptr : *const ()) {
  if env.cells.contains_key(&id) {
    panic!("{} already defined", id);
  }
  env.cells.insert(id, CellValue { e, t, ptr });
}

fn env_alloc_global(mut env : Env, e : Expr, name : Symbol, t : TypeHandle) -> *mut () {
  let id = DefCell(name);
  if env.cells.contains_key(&id) {
    panic!("def {} already defined", id);
  }
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  env.cells.insert(id, CellValue { e, t, ptr });
  ptr
}

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let sym = to_symbol(e.st, s);
  let p = env_alloc_global(e, e.builtin_dummy_expr, sym, t);
  unsafe {
    *(p as *mut u64) = v;
  }
}

fn region_symbol(env : Env) -> Symbol {
  to_symbol(env.st, "region")
}

pub fn set_active_definition(mut env : Env, def : Option<Symbol>) {
  env.active_definition = def;
}

pub fn register_signal(mut env : Env, id : SignalId) {
  let name = env.active_definition.expect("can't register a signal; no active definition");
  let signals = env.signals.entry(name).or_insert_with(|| vec![]);
  signals.push(id);
}

pub fn get_global<T : Copy>(e : Env, sym : Symbol, t : TypeHandle) -> Option<Ptr<T>> {
  if let Some(entry) = e.cells.get(&DefCell(sym)) {
    if entry.t == t {
      return Some(Ptr::from_ptr(entry.ptr as *mut T));
    }
  }
  None
}

pub fn get_global_value<T : Copy>(e : Env, sym : Symbol, t : TypeHandle) -> Option<T> {
  get_global::<T>(e, sym, t).map(|v| *v)
}

pub fn get_global_type(e : Env, sym : Symbol) -> Option<TypeHandle> {
  e.cells.get(&DefCell(sym)).map(|entry| entry.t)
}

pub fn get_const_value(e : Env, expr : Expr) -> Option<CellValue> {
  e.cells.get(&ConstCell(expr)).cloned()
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

impl fmt::Display for CellId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DefCell(name) => write!(f, "def {}", name),
      ConstCell(expr) => write!(f, "const expr ({})", expr.loc()),
    }
  }
}

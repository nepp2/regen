/// Defines the environment (the global hashmap that defs are added to)

use crate::{event_loop::{self, EventLoop, SignalId}, ffi_libs::*, parse::CodeModule, perm_alloc::{Ptr, perm}, symbols::{Symbol, SymbolTable, to_symbol}, types::{self, TypeHandle, CoreTypes, core_types }};

use std::collections::HashMap;

/// Environment for interpreter
#[derive(Clone)]
pub struct Environment {
  values : HashMap<Symbol, EnvEntry>,
  signals : HashMap<Symbol, Vec<SignalId>>,
  pub st : SymbolTable,
  pub c : CoreTypes,
  pub active_definition : Option<Symbol>,
  pub internal_module : Ptr<CodeModule>,
}

pub type Env = Ptr<Environment>;

#[derive(Clone)]
pub struct EnvEntry {
  pub ptr : *const (),
  pub tag : TypeHandle,
  pub module : Ptr<CodeModule>,
}

pub fn unload_def(mut env : Env, name : Symbol) {
  let el = get_event_loop(env);
  if let Some(signals) = env.signals.get(&name) {
    for &id in signals {
      event_loop::destroy_signal(el, id);
    }
  }
  env.values.remove(&name);
  env.signals.remove(&name);
}

pub fn get_entry(env : &Env, name : Symbol) -> Option<&EnvEntry> {
  env.values.get(&name)
}

pub fn insert_entry(mut env : Env, module : Ptr<CodeModule>, name : Symbol, tag : TypeHandle, ptr : *const ()) {
  if env.values.contains_key(&name) {
    panic!("def {} already defined", name);
  }
  env.values.insert(name, EnvEntry { ptr, tag, module });
}

pub fn env_alloc_global(mut env : Env, module : Ptr<CodeModule>, name : Symbol, tag : TypeHandle) -> *mut () {
  if env.values.contains_key(&name) {
    panic!("global {} already defined", name);
  }
  let layout = std::alloc::Layout::from_size_align(tag.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  env.values.insert(name, EnvEntry { ptr, tag, module });
  ptr
}

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let sym = to_symbol(e.st, s);
  let p = env_alloc_global(e, e.internal_module, sym, t);
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
  if let Some(entry) = e.values.get(&sym) {
    if entry.tag == t {
      return Some(Ptr::from_ptr(entry.ptr as *mut T));
    }
  }
  None
}

pub fn get_global_value<T : Copy>(e : Env, sym : Symbol, t : TypeHandle) -> Option<T> {
  get_global::<T>(e, sym, t).map(|v| *v)
}

pub fn get_global_type(e : Env, sym : Symbol) -> Option<TypeHandle> {
  e.values.get(&sym).map(|entry| entry.tag)
}

pub fn get_event_loop(e : Env) -> Ptr<EventLoop> {
  let sym = to_symbol(e.st, "event_loop");
  let entry = e.values.get(&sym).unwrap();
  unsafe { *(entry.ptr as *mut Ptr<EventLoop>) }
}

pub fn new_env(st : SymbolTable) -> Env {
  let internal_module = perm(CodeModule {
    code: "".into(),
    name: "__internal".into(),
  });
  let env = perm(Environment {
    values: HashMap::new(),
    signals: HashMap::new(),
    st,
    c: core_types(st),
    active_definition: None,
    internal_module,
  });
  let event_loop = event_loop::create_event_loop();
  let c = &env.c;
  for (n, t) in &c.core_types {
    define_global(env, n, Ptr::to_u64(*t), env.c.type_tag);
  }

  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);

  define_global(env, "env", Ptr::to_u64(env), void_ptr);

  define_global(env, "region", 0, void_ptr);

  define_global(env, "event_loop", Ptr::to_u64(event_loop), void_ptr);
  load_ffi_libs(env);

  env
}


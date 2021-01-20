/// Defines the environment (the global hashmap that defs are added to)

use crate::{
  event_loop::EventLoop,
  region::{Region, region_alloc, create_region, free_region},
  types,
  symbols::{Symbol, SymbolTable, to_symbol}
};
use types::{ TypeHandle, CoreTypes, core_types };
use crate::ffi_libs::*;
use crate::perm_alloc::{Perm, perm};

use std::collections::HashMap;

/// Environment for interpreter
#[derive(Clone)]
pub struct Environment {
  values : HashMap<Symbol, EnvEntry>,
  pub st : SymbolTable,
  pub c : CoreTypes,
}

pub type Env = Perm<Environment>;

#[derive(Copy, Clone)]
pub struct EnvEntry {
  pub ptr : *mut (),
  pub tag : TypeHandle,
  pub region : Perm<Region>,
}

pub fn unload_def(mut env : Env, name : Symbol) {
  let entry = env.values.get(&name).unwrap();
  free_region(entry.region);
  env.values.remove(&name);
}

pub fn get_entry(env : &Env, name : Symbol) -> Option<&EnvEntry> {
  env.values.get(&name)
}

pub fn env_alloc_global(mut env : Env, name : Symbol, tag : TypeHandle) -> (*mut (), Perm<Region>) {
  let region = create_region();
  let ptr = region_alloc(region, tag.size_of);
  if env.values.contains_key(&name) {
    panic!("global {} already defined", name);
  }
  env.values.insert(name, EnvEntry { ptr, tag, region });
  (ptr, region)
}

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let sym = to_symbol(e.st, s);
  let (p, _) = env_alloc_global(e, sym, t);
  unsafe {
    *(p as *mut u64) = v;
  }
}

fn region_symbol(env : Env) -> Symbol {
  to_symbol(env.st, "region")
}

pub fn set_active_region(mut env : Env, r : Perm<Region>) {
  let sym = region_symbol(env);
  if !env.values.contains_key(&sym) {
    let void_ptr = types::pointer_type(env.c.void_tag);
    env_alloc_global(env, sym, void_ptr);
  }
  let entry = env.values.get_mut(&sym).unwrap();
  unsafe {
    *(entry.ptr as *mut Perm<Region>) = r;
  }
}

pub fn get_active_region(mut env : Env) -> Perm<Region> {
  let sym = region_symbol(env);
  let entry = env.values.get_mut(&sym).unwrap();
  unsafe {
    *(entry.ptr as *mut Perm<Region>)
  }
}

pub fn get_global<T : Copy>(e : Env, sym : Symbol, t : TypeHandle) -> Option<Perm<T>> {
  if let Some(entry) = e.values.get(&sym) {
    if entry.tag == t {
      return Some(Perm::from_ptr(entry.ptr as *mut T));
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

pub fn get_event_loop(e : Env) -> Perm<EventLoop> {
  let sym = to_symbol(e.st, "event_loop");
  let entry = e.values.get(&sym).unwrap();
  unsafe { *(entry.ptr as *mut Perm<EventLoop>) }
}

pub fn new_env(st : SymbolTable) -> Env {
  let env = perm(Environment {
    values: Default::default(),
    st,
    c: core_types(st),
  });
  let event_loop = perm(EventLoop::new());
  let c = &env.c;
  for (n, t) in &c.core_types {
    define_global(env, n, Perm::to_u64(*t), env.c.type_tag);
  }

  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);

  define_global(env, "env", Perm::to_u64(env), void_ptr);

  define_global(env, "region", 0, void_ptr);

  define_global(env, "event_loop", Perm::to_u64(event_loop), void_ptr);
  load_ffi_libs(env);

  env
}


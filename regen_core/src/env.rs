/// Defines the environment (the global hashmap that defs are added to)

use crate::{event_loop::EventLoop, region::Region, symbols::{Symbol, SymbolTable, to_symbol}};
use crate::types;
use types::{ TypeHandle, CoreTypes, core_types };
use crate::ffi_libs::*;
use crate::perm_alloc::{Perm, perm};

use std::collections::HashMap;

/// Environment for interpreter
#[derive(Clone)]
pub struct Environment {
  pub values : HashMap<Symbol, EnvEntry>,
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

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let sym = to_symbol(e.st, s);
  let p = env_alloc_global(e, sym, t);
  unsafe {
    *(p as *mut u64) = v;
  }
}

pub fn set_active_region(e : Env, sym : Symbol) {
  
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

  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);

  define_global(env, "env", Perm::to_u64(env), void_ptr);

  define_global(env, "region", 0, void_ptr);

  define_global(env, "event_loop", Perm::to_u64(event_loop), void_ptr);
  load_ffi_libs(env);

  env
}


/// Defines the environment (the global hashmap that defs are added to)

use crate::symbols::{Symbol, SymbolTable, to_symbol};
use crate::types::{Type, CoreTypes, core_types};
use crate::parse;
use parse::{Node, NodeContent};
use crate::perm_alloc::{PermSlice, perm_slice};
use std::collections::HashMap;

/// Environment for interpreter
pub struct Env {
  pub values : HashMap<Symbol, EnvEntry>,
  pub st : SymbolTable,
  pub c : CoreTypes,
}

#[derive(Copy, Clone)]
pub struct EnvEntry {
  pub value : u64,
  pub tag : Type,
}

impl Env {
  pub fn get(&self, name : Symbol) -> Option<&EnvEntry> {
    self.values.get(&name)
  }

  pub fn get_str(&self, name : &str) -> Option<&EnvEntry> {
    let name = to_symbol(self.st, name);
    self.get(name)
  }

  pub fn insert(&mut self, name : Symbol, value : u64, tag : Type) {
    self.values.insert(name, EnvEntry {value, tag });
  }

  pub fn insert_str(&mut self, name : &str, value : u64, tag : Type) {
    let name = to_symbol(self.st, name);
    self.insert(name, value, tag);
  }
}

pub extern "C" fn c_add(a : u64, b : u64) -> u64 {
  a + b
}

#[repr(C)]
pub struct TestRect {
  x : u32,
  y : u32,
  w : u32,
  h : u32,
}

pub extern "C" fn test_struct(r : TestRect) -> bool {
  r.x == 1 && r.y == 2 && r.w == 3 && r.h == 4
}

extern {
  pub fn malloc(size: usize) -> *mut u8;
  pub fn free(ptr: *mut u8);
  pub fn memcpy(dest : *mut u8, src: *const u8, count : usize) -> *mut u8;
}

pub extern "C" fn env_insert(env : &mut Env, sym : Symbol, value : u64) {
  env.insert(sym, value, env.c.u64_tag);
}

pub extern "C" fn env_get(env : &Env, sym : Symbol) -> u64 {
  env.get(sym).unwrap().value
}

pub extern "C" fn print_symbol(sym : Symbol) {
  println!("symbol: {}", sym)
}

pub extern "C" fn node_children(out : &mut PermSlice<Node>, node : Node) {
  let s = match node.content {
    NodeContent::List(l) => l,
    _ => perm_slice(&[]),
  };
  *out = s;
}

pub extern "C" fn display_node(node : Node) {
  println!("{}", node);
}

pub fn new_env(st : SymbolTable) -> Box<Env> {
  let env_ptr = Box::into_raw(Box::new(Env {
    values: Default::default(),
    st,
    c: core_types(st),
  }));
  let mut env = unsafe { Box::from_raw(env_ptr) };
  for &t in &env.c.core_types {
    env.values.insert(t.get().id,
      EnvEntry { value: t.as_u64(), tag: env.c.type_tag });
  }
  env.insert_str("c_add", c_add as u64, env.c.u64_tag);
  env.insert_str("test_struct", test_struct as u64, env.c.u64_tag);
  env.insert_str("malloc", malloc as u64, env.c.u64_tag);
  env.insert_str("free", free as u64, env.c.u64_tag);
  env.insert_str("memcpy", memcpy as u64, env.c.u64_tag);
  env.insert_str("env", env_ptr as u64, env.c.u64_tag);
  env.insert_str("env_insert", env_insert as u64, env.c.u64_tag);
  env.insert_str("env_get", env_get as u64, env.c.u64_tag);
  env.insert_str("print_symbol", print_symbol as u64, env.c.u64_tag);
  env.insert_str("node_children", node_children as u64, env.c.u64_tag);
  env.insert_str("display_node", display_node as u64, env.c.u64_tag);
  env
}


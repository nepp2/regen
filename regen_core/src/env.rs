/// Defines the environment (the global hashmap that defs are added to)

use crate::symbols::{Symbol, SymbolTable, to_symbol};
use crate::types::{Type, TypeHandle, CoreTypes, core_types, c_function_type};
use crate::parse;
use crate::interpret;
use parse::{Node, NodeInfo, NodeContent, SrcLocation};
use crate::perm_alloc::{Perm, PermSlice, perm_slice, perm};
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
  pub value : u64,
  pub tag : Type,
}

impl Environment {
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

pub extern "C" fn fail(s : Symbol) -> bool {
  panic!("failed with symbol {}", s)
}

extern {
  pub fn malloc(size: usize) -> *mut u8;
  pub fn free(ptr: *mut u8);
  pub fn memcpy(dest : *mut u8, src: *const u8, count : usize) -> *mut u8;
}

pub extern "C" fn env_insert(mut env : Env, sym : Symbol, value : u64, t : Type) {
  env.insert(sym, value, t);
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

pub extern "C" fn node_from_list(list : &PermSlice<Node>) -> Node {
  let info = NodeInfo {
    loc: SrcLocation::zero(),
    content: NodeContent::List(perm_slice(list.as_slice())),
  };
  perm(info)
}

pub extern "C" fn node_from_symbol(s : Symbol) -> Node {
  let info = NodeInfo {
    loc: SrcLocation::zero(),
    content: NodeContent::Sym(s),
  };
  perm(info)
}

pub extern "C" fn node_as_symbol(n : Node) -> Symbol {
  n.as_symbol()
}

pub extern "C" fn node_display(node : Node) {
  println!("{}", node);
}

const PRELUDE : &'static str = std::include_str!("../../examples/prelude.gen");

pub extern "C" fn load_prelude(env : Env) {
  interpret::interpret_file(PRELUDE, env);
}

pub extern "C" fn eval(env : Env, n : Node) {
  interpret::interpret_node(n, env);
}

pub fn new_env(st : SymbolTable) -> Env {
  let mut env = perm(Environment {
    values: Default::default(),
    st,
    c: core_types(st),
  });
  let e = env;
  let c = &e.c;
  for (n, t) in &c.core_types {
    let type_handle = TypeHandle::alloc_type(*t);
    let e = EnvEntry { value: type_handle.as_u64(), tag: env.c.type_tag };
    env.values.insert(*n, e);
  }

  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;

  env.insert_str("c_add", c_add as u64,
    c_function_type(c, &[u64, u64], u64));

  env.insert_str("test_struct", test_struct as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("fail", fail as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("malloc", malloc as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("free", free as u64,
    c_function_type(c, &[u64], void));

  env.insert_str("memcpy", memcpy as u64,
    c_function_type(c, &[u64, u64, u64], void));

  env.insert_str("env", Perm::to_ptr(e) as u64, u64);

  env.insert_str("env_insert", env_insert as u64,
    c_function_type(c, &[u64, u64, u64, u64], void));

  env.insert_str("env_get", env_get as u64, 
    c_function_type(c, &[u64, u64], u64));

  env.insert_str("print_symbol", print_symbol as u64,
    c_function_type(c, &[u64], void));

  env.insert_str("node_children", node_children as u64,
    c_function_type(c, &[u64, u64], void));

  env.insert_str("node_display", node_display as u64,
    c_function_type(c, &[u64], void));

  env.insert_str("node_from_list", node_from_list as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("node_from_symbol", node_from_symbol as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("node_as_symbol", node_as_symbol as u64,
    c_function_type(c, &[u64], u64));

  env.insert_str("load_prelude", load_prelude as u64,
    c_function_type(c, &[u64], void));

  env.insert_str("eval", eval as u64,
    c_function_type(c, &[u64, u64], void));

  env
}


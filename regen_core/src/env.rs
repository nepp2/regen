/// Defines the environment (the global hashmap that defs are added to)

use crate::symbols::{Symbol, SymbolTable, to_symbol};
use crate::types;
use types::{ TypeHandle, CoreTypes, core_types, c_function_type };
use crate::parse;
use crate::interpret;
use parse::{Node, NodeInfo, NodeContent, SrcLocation, node_shape, NodeShape::*};
use crate::perm_alloc::{Perm, PermSlice, perm_slice, perm_slice_from_vec, perm};
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
  pub tag : TypeHandle,
}

impl Environment {
  pub fn get(&self, name : Symbol) -> Option<&EnvEntry> {
    self.values.get(&name)
  }

  pub fn get_str(&self, name : &str) -> Option<&EnvEntry> {
    let name = to_symbol(self.st, name);
    self.get(name)
  }

  pub fn insert(&mut self, name : Symbol, value : u64, tag : TypeHandle) {
    self.values.insert(name, EnvEntry {value, tag });
  }

  pub fn insert_str(&mut self, name : &str, value : u64, tag : TypeHandle) {
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

pub extern "C" fn test_tuple(r : TestRect) -> bool {
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

pub extern "C" fn env_insert(mut env : Env, sym : Symbol, value : u64, t : TypeHandle) {
  env.insert(sym, value, t);
}

pub extern "C" fn env_get(env : Env, sym : Symbol) -> u64 {
  env.get(sym).unwrap().value
}

pub extern "C" fn symbol_display(sym : Symbol) {
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

pub extern "C" fn node_from_literal(v : u64) -> Node {
  let info = NodeInfo {
    loc: SrcLocation::zero(),
    content: NodeContent::Literal(v),
  };
  perm(info)
}

pub extern "C" fn node_as_symbol(n : Node) -> Symbol {
  n.as_symbol()
}

pub extern "C" fn node_display(node : Node) {
  println!("{}", node);
}

pub extern "C" fn eval(env : Env, n : Node) -> u64 {
  interpret::interpret_node(n, env)
}

pub extern "C" fn calculate_packed_field_offsets(
  field_types : &PermSlice<TypeHandle>,
  field_offsets : &mut PermSlice<u64>,
) -> u64
{
  let (offsets, size_of) = types::calculate_packed_field_offsets(field_types.as_slice());
  *field_offsets = perm_slice_from_vec(offsets);
  size_of
}

pub extern "C" fn template_quote(n : Node, args : &PermSlice<Node>) -> Node {
  fn template(n : Node, args : &[Node], next_arg : &mut usize) -> Node {
    match node_shape(&n) {
      Atom(_) | Literal(_) => n,
      Command("$", [_]) => {
        let new_e = args[*next_arg];
        *next_arg += 1;
        new_e
      }
      _ => {
        let mut children = vec![];
        for &c in n.children() {
          children.push(template(c, args, next_arg));
        }
        let loc = n.loc;
        let content = NodeContent::List(perm_slice_from_vec(children));
        perm(NodeInfo { loc, content })
      },
    }
  }
  template(n, args.as_slice(), &mut 0)
}

pub extern "C" fn type_display(t : TypeHandle) {
  println!("{}", t);
}

pub extern "C" fn debug_line_start(n: Node) {
  print!("{}: ", n);
}

pub fn new_env(st : SymbolTable) -> Env {
  let mut env = perm(Environment {
    values: Default::default(),
    st,
    c: core_types(),
  });
  let e = env;
  let c = &e.c;
  for (n, t) in &c.core_types {
    let e = EnvEntry { value: Perm::to_u64(*t), tag: env.c.type_tag };
    let n = to_symbol(st, *n);
    env.values.insert(n, e);
  }

  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;

  env.insert_str("c_add", c_add as u64,
    c_function_type(&[u64, u64], u64));

  env.insert_str("test_tuple", test_tuple as u64,
    c_function_type(&[u64], u64));

  env.insert_str("fail", fail as u64,
    c_function_type(&[u64], u64));

  env.insert_str("malloc", malloc as u64,
    c_function_type(&[u64], u64));

  env.insert_str("free", free as u64,
    c_function_type(&[u64], void));

  env.insert_str("memcpy", memcpy as u64,
    c_function_type(&[u64, u64, u64], void));

  env.insert_str("env", Perm::to_ptr(e) as u64, u64);

  env.insert_str("env_insert", env_insert as u64,
    c_function_type(&[u64, u64, u64, u64], void));

  env.insert_str("env_get", env_get as u64, 
    c_function_type(&[u64, u64], u64));

  env.insert_str("symbol_display", symbol_display as u64,
    c_function_type(&[u64], void));

  env.insert_str("node_children_c", node_children as u64,
    c_function_type(&[u64, u64], void));

  env.insert_str("node_display", node_display as u64,
    c_function_type(&[u64], void));

  env.insert_str("node_from_list", node_from_list as u64,
    c_function_type(&[u64], u64));

  env.insert_str("node_from_symbol", node_from_symbol as u64,
    c_function_type(&[u64], u64));

  env.insert_str("node_from_literal", node_from_literal as u64,
    c_function_type(&[u64], u64));

  env.insert_str("node_as_symbol", node_as_symbol as u64,
    c_function_type(&[u64], u64));

  env.insert_str("eval", eval as u64,
    c_function_type(&[u64, u64], u64));

  env.insert_str("template_quote", template_quote as u64,
    c_function_type(&[u64, u64], u64));

  env.insert_str("calculate_packed_field_offsets", calculate_packed_field_offsets as u64,
    c_function_type(&[u64, u64], u64));

  env.insert_str("type_display", type_display as u64,
    c_function_type(&[u64], void));

  env.insert_str("debug_line_start", debug_line_start as u64,
    c_function_type(&[u64], void));

  env
}


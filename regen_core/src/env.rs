/// Defines the environment (the global hashmap that defs are added to)

use crate::symbols::{Symbol, SymbolTable, to_symbol};
use crate::types;
use types::{ TypeHandle, CoreTypes, core_types, c_function_type };
use crate::parse;
use crate::interpret;
use parse::{Node, NodeInfo, NodeContent, NodeLiteral, SrcLocation, node_shape, NodeShape::*};
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
  pub ptr : *mut (),
  pub tag : TypeHandle,
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

pub extern "C" fn env_alloc_global(mut env : Env, name : Symbol, tag : TypeHandle) -> *mut () {
  let layout = std::alloc::Layout::from_size_align(tag.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) } as *mut ();
  if env.values.contains_key(&name) {
    panic!("global {} already defined", name);
  }
  env.values.insert(name, EnvEntry {ptr, tag });
  ptr
}

pub extern "C" fn env_get_global_ptr(env : Env, sym : Symbol) -> *mut () {
  env.values.get(&sym).unwrap().ptr
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
    content: NodeContent::Literal(NodeLiteral::U64(v)),
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

pub extern "C" fn template_quote(n : Node, args_ptr : *const Node, num_args : u64) -> Node {
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
  let args = unsafe { std::slice::from_raw_parts(args_ptr, num_args as usize) };
  template(n, args, &mut 0)
}

pub extern "C" fn type_sizeof(t : TypeHandle) -> u64 {
  t.size_of
}

pub extern "C" fn type_display(t : TypeHandle) {
  println!("{}", t);
}

pub fn define_global(e : Env, s : &str, v : u64, t : TypeHandle) {
  let sym = to_symbol(e.st, s);
  let p = env_alloc_global(e, sym, t);
  unsafe {
    *(p as *mut u64) = v;
  }
}

pub fn get_global_value<T : Copy>(e : Env, sym : Symbol, t : TypeHandle) -> Option<T> {
  if let Some(entry) = e.values.get(&sym) {
    if entry.tag == t {
      unsafe {
        return Some(*(entry.ptr as *mut T))
      }
    }
  }
  None
}

pub fn get_global_type(e : Env, sym : Symbol) -> Option<TypeHandle> {
  e.values.get(&sym).map(|entry| entry.tag)
}

pub fn new_env(st : SymbolTable) -> Env {
  let env = perm(Environment {
    values: Default::default(),
    st,
    c: core_types(),
  });
  let e = env;
  let c = &e.c;
  for (n, t) in &c.core_types {
    define_global(e, n, Perm::to_u64(*t), env.c.type_tag);
  }

  let u64 = env.c.u64_tag;
  let void = env.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let node = env.c.node_tag;
  let type_tag = env.c.type_tag;

  define_global(e, "c_add", c_add as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "test_tuple", test_tuple as u64,
    c_function_type(&[u64], u64));

  define_global(e, "fail", fail as u64,
    c_function_type(&[u64], u64));

  define_global(e, "malloc", malloc as u64,
    c_function_type(&[u64], void_ptr));

  define_global(e, "free", free as u64,
    c_function_type(&[void_ptr], void));

  define_global(e, "memcpy", memcpy as u64,
    c_function_type(&[void_ptr, void_ptr, u64], void));  

  define_global(e, "env", Perm::to_u64(e), u64);

  define_global(e, "env_alloc_global", env_alloc_global as u64,
    c_function_type(&[u64, u64, type_tag], void_ptr));

  define_global(e, "env_get_global_ptr", env_get_global_ptr as u64,
    c_function_type(&[u64, u64], void_ptr));

  define_global(e, "symbol_display", symbol_display as u64,
    c_function_type(&[u64], void));

  define_global(e, "node_children_c", node_children as u64,
    c_function_type(&[u64, u64], void));

  define_global(e, "node_display", node_display as u64,
    c_function_type(&[u64], void));

  define_global(e, "node_from_list", node_from_list as u64,
    c_function_type(&[u64], u64));

  define_global(e, "node_from_symbol", node_from_symbol as u64,
    c_function_type(&[u64], u64));

  define_global(e, "node_from_literal", node_from_literal as u64,
    c_function_type(&[u64], u64));

  define_global(e, "node_as_symbol", node_as_symbol as u64,
    c_function_type(&[u64], u64));

  define_global(e, "eval", eval as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "template_quote", template_quote as u64,
    c_function_type(&[node, types::pointer_type(node), u64], node));

  define_global(e, "calculate_packed_field_offsets", calculate_packed_field_offsets as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "type_sizeof", type_sizeof as u64,
    c_function_type(&[type_tag], u64));

  define_global(e, "type_display", type_display as u64,
    c_function_type(&[u64], void));

  env
}


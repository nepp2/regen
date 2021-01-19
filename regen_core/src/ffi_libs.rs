use crate::{
  env::{Env, EnvEntry, define_global},
  event_loop,
  interpret,
  node_macros::template,
  perm_alloc,
  region,
  sexp,
  symbols::Symbol,
  types,
  types::{TypeHandle, c_function_type}
};

use sexp::{Node, NodeInfo, NodeContent, NodeLiteral, SrcLocation};
use perm_alloc::{PermSlice, perm_slice, perm_slice_from_vec, perm};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct RegenString {
  pub ptr : *const u8,
  pub len : u64,
}

impl RegenString {
  pub fn as_str(&self) -> &str {
    unsafe { 
      let s = std::slice::from_raw_parts(self.ptr, self.len as usize);
      std::str::from_utf8_unchecked(s)
    }
  }
}

pub fn from_string(s : String) -> RegenString {
  let len = s.len() as u64;
  let mut bytes = s.into_bytes();
  bytes.push(0);
  let ptr = bytes.as_ptr();
  std::mem::forget(bytes);
  RegenString { ptr, len }
}

pub extern "C" fn fail(s : Symbol) -> bool {
  panic!("failed with symbol {}", s)
}

extern {
  pub fn malloc(size: usize) -> *mut u8;
  pub fn free(ptr: *mut u8);
  pub fn memcpy(dest : *mut u8, src: *const u8, count : usize) -> *mut u8;
  pub fn memset(dest : *mut u8, val: i32, count : usize) -> *mut u8;
}

pub extern "C" fn env_alloc_global(mut env : Env, name : Symbol, tag : TypeHandle) -> *mut () {
  let region = region::create_region();
  let ptr = region::region_alloc(region, tag.size_of);
  if env.values.contains_key(&name) {
    panic!("global {} already defined", name);
  }
  env.values.insert(name, EnvEntry { ptr, tag, region });
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
  let args = unsafe { std::slice::from_raw_parts(args_ptr, num_args as usize) };
  template(n, args)
}

pub extern "C" fn type_sizeof(t : TypeHandle) -> u64 {
  t.size_of
}

pub extern "C" fn type_display(t : TypeHandle) {
  println!("{}", t);
}

pub fn load_ffi_libs(e : Env) {
  let u64 = e.c.u64_tag;
  let u32 = e.c.u32_tag;
  let void = e.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let node = e.c.node_tag;
  let type_tag = e.c.type_tag;

  define_global(e, "fail", fail as u64,
    c_function_type(&[u64], u64));

  define_global(e, "malloc", malloc as u64,
    c_function_type(&[u64], void_ptr));

  define_global(e, "free", free as u64,
    c_function_type(&[void_ptr], void));

  define_global(e, "memcpy", memcpy as u64,
    c_function_type(&[void_ptr, void_ptr, u64], void_ptr));

  define_global(e, "memset", memset as u64,
    c_function_type(&[void_ptr, u32, u64], void_ptr));

  define_global(e, "env_alloc_global", env_alloc_global as u64,
    c_function_type(&[void_ptr, u64, type_tag], void_ptr));

  define_global(e, "env_get_global_ptr", env_get_global_ptr as u64,
    c_function_type(&[void_ptr, u64], void_ptr));

  define_global(e, "tick_stream_ffi", event_loop::ffi::tick_stream as u64,
    c_function_type(&[void_ptr, u64], u64));

  define_global(e, "state_stream_ffi", event_loop::ffi::state_stream as u64,
    c_function_type(&[void_ptr, u64, type_tag, void_ptr, void_ptr, void_ptr], u64));

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
    c_function_type(&[void_ptr, u64], u64));

  define_global(e, "template_quote", template_quote as u64,
    c_function_type(&[node, types::pointer_type(node), u64], node));

  define_global(e, "calculate_packed_field_offsets", calculate_packed_field_offsets as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "type_sizeof", type_sizeof as u64,
    c_function_type(&[type_tag], u64));

  define_global(e, "type_display", type_display as u64,
    c_function_type(&[u64], void));
}


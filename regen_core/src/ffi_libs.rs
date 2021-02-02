use std::hash::{Hash, Hasher};

use crate::{env, event_loop::ffi::*, expr_macros::template, parse::Expr, perm_alloc, symbols::Symbol, types, types::{TypeHandle, c_function_type, function_type}};

use env::{Env, define_global};
use perm_alloc::{SlicePtr, perm_slice_from_vec};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct RegenString {
  pub ptr : *const u8,
  pub len : u64,
}

impl PartialEq for RegenString {
  fn eq(&self, rhs : &Self) -> bool {
    self.as_str() == rhs.as_str()
  }
}
impl Eq for RegenString {}
impl Hash for RegenString {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_str().hash(state)
  }
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

pub extern "C" fn env_get_global_ptr(env : Env, sym : Symbol) -> *mut () {
  env::get_entry(&env, sym).unwrap().ptr
}

pub extern "C" fn symbol_display(sym : Symbol) {
  println!("symbol: {}", sym)
}

// TODO: fix these functions to work with Expr instead

// pub extern "C" fn node_children(out : &mut SlicePtr<Node>, expr : Node) {
//   let s = match expr.content {
//     NodeContent::List(l) => l,
//     _ => perm_slice(&[]),
//   };
//   *out = s;
// }

// pub extern "C" fn node_from_list(list : &SlicePtr<Node>) -> Node {
//   let info = NodeInfo {
//     loc: SrcLocation::zero(),
//     content: NodeContent::List(perm_slice(list.as_slice())),
//   };
//   perm(info)
// }

// pub extern "C" fn node_from_symbol(s : Symbol) -> Node {
//   let info = NodeInfo {
//     loc: SrcLocation::zero(),
//     content: NodeContent::Sym(s),
//   };
//   perm(info)
// }

// pub extern "C" fn node_from_literal(v : i64) -> Node {
//   let info = NodeInfo {
//     loc: SrcLocation::zero(),
//     content: NodeContent::Literal(NodeLiteral::I64(v)),
//   };
//   perm(info)
// }

// pub extern "C" fn node_as_symbol(n : Node) -> Symbol {
//   n.as_symbol()
// }

pub extern "C" fn node_display(expr : Expr) {
  println!("{}", expr.loc.src_snippet());
}

pub extern "C" fn calculate_packed_field_offsets(
  field_types : &SlicePtr<TypeHandle>,
  field_offsets : &mut SlicePtr<u64>,
) -> u64
{
  let (offsets, size_of) = types::calculate_packed_field_offsets(field_types.as_slice());
  *field_offsets = perm_slice_from_vec(offsets);
  size_of
}

pub extern "C" fn template_quote(e : Expr, args_ptr : *const Expr, num_args : u64) -> Expr {
  let args = unsafe { std::slice::from_raw_parts(args_ptr, num_args as usize) };
  template(e, args)
}

pub extern "C" fn type_sizeof(t : TypeHandle) -> u64 {
  t.size_of
}

pub extern "C" fn type_display(t : TypeHandle) {
  println!("{}", t);
}

pub fn load_ffi_libs(e : Env) {
  let bool_type = e.c.bool_tag;
  let u64 = e.c.u64_tag;
  let i64 = u64;
  let u32 = e.c.u32_tag;
  let void = e.c.void_tag;
  let void_ptr = types::pointer_type(void);
  let stream_ptr = void_ptr;
  let env_ptr = void_ptr;
  let event_loop_ptr = void_ptr;
  let expr = e.c.expr_tag;
  let type_tag = e.c.type_tag;

  // ----------- Bind core system functions ------------

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

  define_global(e, "env_get_global_ptr", env_get_global_ptr as u64,
    c_function_type(&[void_ptr, u64], void_ptr));

  // ----------- Bind stream functions ------------

  define_global(e, "register_tick_stream", register_tick_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, i64],
        stream_ptr));

  let update_fn_type =
    function_type(&[void_ptr, void_ptr], void);
  define_global(e, "register_state_stream", register_state_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, stream_ptr, type_tag, void_ptr, update_fn_type],
        stream_ptr));

  let poll_fn_type =
    function_type(&[void_ptr, void_ptr, void_ptr], bool_type);
  define_global(e, "register_poll_stream", register_poll_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, stream_ptr, type_tag, void_ptr, type_tag, poll_fn_type],
        stream_ptr));

  let map_fn_type =
    function_type(&[void_ptr, void_ptr], void);
  define_global(e, "register_map_stream", register_map_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, stream_ptr, type_tag, map_fn_type],
        stream_ptr));

  define_global(e, "register_merge_stream", register_merge_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, stream_ptr, stream_ptr, type_tag],
        stream_ptr));

  define_global(e, "register_sample_stream", register_sample_stream as u64,
    c_function_type(
      &[env_ptr, event_loop_ptr, stream_ptr, stream_ptr, type_tag],
        stream_ptr));

  // ----------- Bind language introspection functions ------------

  define_global(e, "symbol_display", symbol_display as u64,
    c_function_type(&[u64], void));

  let TODO = (); // fix these functions
  
  // define_global(e, "node_children_c", node_children as u64,
  //   c_function_type(&[u64, u64], void));

  // define_global(e, "node_display", node_display as u64,
  //   c_function_type(&[u64], void));

  // define_global(e, "node_from_list", node_from_list as u64,
  //   c_function_type(&[u64], u64));

  // define_global(e, "node_from_symbol", node_from_symbol as u64,
  //   c_function_type(&[u64], u64));

  // define_global(e, "node_from_literal", node_from_literal as u64,
  //   c_function_type(&[u64], u64));

  // define_global(e, "node_as_symbol", node_as_symbol as u64,
  //   c_function_type(&[u64], u64));

  define_global(e, "template_quote", template_quote as u64,
    c_function_type(&[expr, types::pointer_type(expr), u64], expr));

  define_global(e, "calculate_packed_field_offsets", calculate_packed_field_offsets as u64,
    c_function_type(&[u64, u64], u64));

  define_global(e, "type_sizeof", type_sizeof as u64,
    c_function_type(&[type_tag], u64));

  define_global(e, "type_display", type_display as u64,
    c_function_type(&[u64], void));
}


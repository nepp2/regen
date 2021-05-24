
use std::fmt::Debug;

use compiler::{self, env::{self, CellIdentifier, CellValue, Env}, ffi_libs::RegenString, hotload, perm_alloc::{Ptr, perm}, symbols::{SymbolTable, symbol_table, to_symbol}, types::{self, CoreTypes, TypeHandle, core_types}};
use rusty_fork::rusty_fork_test;

#[derive(Clone, Copy)]
struct TestEnv {
  st : SymbolTable,
  c : Ptr<CoreTypes>,
}

impl TestEnv {
  fn new_env(&self) -> Env {
    env::new_env(self.st, self.c)
  }

  fn eval_single_expr<T>(&self, expr : &str, t : TypeHandle) -> T
    where T : Copy
  {
    let env = self.new_env();
    let code = format!("def a = {}", expr);
    hotload::hotload_module(env, "test", &code);
    *get_def_val(env, "a", t)
  }

  fn assert_single_expr<T>(&self, expr : &str, t : TypeHandle, v : T)
    where T : Debug + PartialEq
  {
    let env = self.new_env();
    let code = format!("def a = {}", expr);
    hotload::hotload_module(env, "test", &code);
    let result_val : &T = get_def_val(env, "a", t);
    assert_eq!(result_val, &v);
  }
}

fn get_cell_value(env : Env, defname : &str) -> CellValue {
  let namespace = env::new_namespace(&[]);
  let sym = to_symbol(env.st, defname);
  let uid = CellIdentifier::DefCell(namespace, sym).uid(env);
  env::get_cell_value(env, uid).unwrap_or_else(|| panic!("value for {} not found", defname))
}

fn assert_def_type(env : Env, defname : &str, t : TypeHandle) {
  let cv = get_cell_value(env, defname);
  assert_eq!(cv.t, t);
}

fn get_def_val<T>(env : Env, defname : &str, t : TypeHandle) -> &T
{
  let cv = get_cell_value(env, defname);
  assert_eq!(cv.t, t);
  unsafe { &*(cv.ptr as *mut T) }
}

fn assert_def_val<T : PartialEq + Debug>(env : Env, defname : &str, t : TypeHandle, v : T)
{
  let def_val : &T = get_def_val(env, defname, t);
  assert_eq!(def_val, &v);
}

fn test_env() -> TestEnv {
  let st = symbol_table();
  let c = perm(core_types(st));
  TestEnv { st, c }
}

rusty_fork_test! {
  #[test]
  fn primitives() {
    let te = test_env();
    te.assert_single_expr("5", te.c.i64_tag, 5);
    te.assert_single_expr("1 + 1", te.c.i64_tag, 2);
    te.assert_single_expr("typeof 5", te.c.type_tag, te.c.i64_tag);
    te.assert_single_expr("typeof u32", te.c.type_tag, te.c.type_tag);
  }

  #[test]
  fn pointers() {
    let te = test_env();
    let code = "
      {
        let p = malloc(8 as u64) as ptr i64;
        *p = 500;
        let v1 = *p;
        let local = 1000;
        let v2 = *(ref local);
        v1 + v2
      }
    ";
    te.assert_single_expr(code, te.c.i64_tag, 1500);
  }

  #[test]
  fn structs() {
    let code = "
      def pos2 = struct { x : i64, y : i64 };
      def v = init pos2(40, 50);
      def x = v.x;
      def y = v.y;
      // field assignment
      def assigned_y = {
        let v2 = v;
        v2.y = 300;
        v2.y
      }
    ";
    let te = test_env();
    let env = te.new_env();
    hotload::hotload_module(env, "test", code);
    assert_def_type(env, "pos2", te.c.type_tag);
    assert_def_val(env, "x", te.c.i64_tag, 40);
    assert_def_val(env, "y", te.c.i64_tag, 50);
    assert_def_val(env, "assigned_y", te.c.i64_tag, 300);
  }
  
  #[test]
  fn strings() {
    let te = test_env();
    let s : RegenString = te.eval_single_expr("\"hello world\"", te.c.string_tag);
    assert_eq!("hello world", s.as_str());
  }

  #[test]
  fn arrays() {
    let code = "
      def a = [1, 2, 3, 4];
      def a0 = a[0];
      def a2 = a[2];
      def a3 = a[3];
      def b2 = {
        let b = a;
        let p = ref b[2];
        *p = 300;
        b[2]
      }
    ";
    let te = test_env();
    let env = te.new_env();
    hotload::hotload_module(env, "test", code);
    let array_type = types::array_type(te.c.i64_tag, 4);
    assert_def_type(env, "a", array_type);
    assert_def_val(env, "a0", te.c.i64_tag, 1);
    assert_def_val(env, "a2", te.c.i64_tag, 3);
    assert_def_val(env, "a3", te.c.i64_tag, 4);
    assert_def_val(env, "b2", te.c.i64_tag, 300);
  }

   
  #[test]
  fn conditions() {
    let te = test_env();
    te.assert_single_expr("if 3 > 5 { 3 } else { 5 }", te.c.i64_tag, 5);
  }

  #[test]
  fn functions() {
    let code = "
      def add = fun(a : i64, b : i64) => i64 {
        a + b
      };
      
      def v = add(6, 7); // 13
    ";
    let te = test_env();
    let env = te.new_env();
    hotload::hotload_module(env, "test", code);
    assert_def_val(env, "v", te.c.i64_tag, 13);
  }
  
  #[test]
  fn labelled_blocks() {
    let code = "
      {
        let v = 0;
        label loop {
          if v > 10 {
            break loop;
          }
          v = v + 1;
          repeat loop;
        }
        v
      }
    ";
    let te = test_env();
    te.assert_single_expr(code, te.c.i64_tag, 11);
  }
  
  #[test]
  fn metaprogramming() {
    let code = "
      def a = #500;
      def b = #400;
      def e = #($a + $b);
      def v = embed e;
    ";
    let te = test_env();
    let env = te.new_env();
    hotload::hotload_module(env, "test", code);
    assert_def_val(env, "v", te.c.i64_tag, 900);
  }

  #[test]
  fn while_loops() {
    let code = "
      {
        let i = 0;
        while i < 10 { i = i + 1 };
        i
      }
    ";
    let te = test_env();
    te.assert_single_expr(code, te.c.i64_tag, 10);
  }

  #[test]
  fn for_loops() {
    let code = "
      {
        let total = 0;
        let vs = [1, 2, 3, 4, 5, 6, 7, 8, 9];
        for i in 0 to array_len(vs) {
          total = total + vs[i];
        }
        total
      }
    ";
    let te = test_env();
    te.assert_single_expr(code, te.c.i64_tag, 45);
  }
}

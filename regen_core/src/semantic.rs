use std::collections::{HashMap, HashSet};

use crate::{parse::{Expr, ExprShape, ExprTag}, perm_alloc::{Ptr, perm}, symbols::Symbol};

#[derive(Copy, Clone, PartialEq)]
pub enum ReferenceType {
  Global, Local,
}

#[derive(Clone)]
pub struct SemanticInfo {
  references : HashMap<u64, (Expr, ReferenceType)>,
}

impl SemanticInfo {
  fn push_ref(&mut self, e : Expr, r : ReferenceType) {
    let id = Ptr::to_u64(e);
    if self.references.contains_key(&id) {
      panic!("multiple instances of expression encountered")
    }
    self.references.insert(id, (e, r));
  }

  pub fn get_ref_type(&self, e : Expr) -> ReferenceType {
    self.references[&Ptr::to_u64(e)].1
  }

  pub fn global_set(&self) -> HashSet<Symbol> {
    let mut globals = HashSet::new();
    for &(e, t) in self.references.values() {
      if t == ReferenceType::Global {
        globals.insert(e.as_symbol());
      }
    }
    globals
  }
}

pub fn get_semantic_info(expr : Expr) -> Ptr<SemanticInfo> {
  let mut info = SemanticInfo { references: HashMap::new() };
  let mut locals = vec![];
  search_expr(&mut info, &mut locals, expr);
  perm(info)
}

fn search_expr(info : &mut SemanticInfo, locals : &mut Vec<Symbol>, expr : Expr) {
  use ExprTag::*;
  use ExprShape::*;
  use ReferenceType::*;
  match expr.shape() {
    Ref(name) => {
      if !expr.ignore_symbol {
        if locals.iter().find(|&&n| n == name).is_some() {
          info.push_ref(expr, Local);
        }
        else {
          info.push_ref(expr, Global);
        }
      }
    }
    List(Let, &[name, _value]) => {
      locals.push(name.as_symbol());
    }
    List(Fun, &[args, _ret, _body]) => {
      let mut function_locals = vec![];
      for a in args.children() {
        if let List(Syntax, &[name, _type_tag]) = a.shape() {
          let n = name.as_symbol();
          function_locals.push(n);
        }
        else { panic!() }
      }
      for &c in expr.children() {
        search_expr(info, &mut function_locals, c);
      }
      return;
    }
    List(Do, exprs) => {
      let local_count = locals.len();
      for &c in exprs {
        search_expr(info, locals, c);
      }
      locals.drain(local_count..);
      return;
    }
    _ => (),
  }
  for &c in expr.children() {
    search_expr(info, locals, c);
  }
}

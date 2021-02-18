use std::collections::{HashMap, HashSet};

use crate::{parse::{Expr, ExprShape, ExprTag}, perm_alloc::{Ptr, perm}, symbols::Symbol};

use ExprTag::*;
use ExprShape::*;
use ReferenceType::*;

#[derive(Copy, Clone, PartialEq)]
pub enum ReferenceType {
  Global, Local,
}

#[derive(Clone)]
pub struct ReferenceInfo {
  references : HashMap<u64, (Expr, ReferenceType)>,
}

impl ReferenceInfo {

  pub fn get_ref_type(&self, e : Expr) -> ReferenceType {
    self.references.get(&Ptr::to_u64(e))
      .unwrap_or_else(|| panic!("no type found for {}", e))
      .1
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

pub fn get_semantic_info(expr : Expr) -> Ptr<ReferenceInfo> {
  let mut references = HashMap::new();
  let mut locals = vec![];
  find_refs(&mut references, &mut locals, expr);
  let info = ReferenceInfo { references };
  perm(info)
}

fn push_ref(refs : &mut HashMap<u64, (Expr, ReferenceType)>, e : Expr, r : ReferenceType) {
  let id = Ptr::to_u64(e);
  if refs.contains_key(&id) {
    panic!("multiple instances of expression encountered")
  }
  refs.insert(id, (e, r));
}

fn find_refs(
  refs : &mut HashMap<u64, (Expr, ReferenceType)>,
  locals : &mut Vec<Symbol>,
  expr : Expr
)
{
  match expr.shape() {
    Sym(name) => {
      if !expr.ignore_symbol {
        if locals.iter().find(|&&n| n == name).is_some() {
          push_ref(refs, expr, Local);
        }
        else {
          push_ref(refs, expr, Global);
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
        find_refs(refs, &mut function_locals, c);
      }
      return;
    }
    List(Do, exprs) => {
      let local_count = locals.len();
      for &c in exprs {
        find_refs(refs, locals, c);
      }
      locals.drain(local_count..);
      return;
    }
    _ => (),
  }
  for &c in expr.children() {
    find_refs(refs, locals, c);
  }
}

/// All of the const expressions in the expr, in topological order
pub fn get_const_exprs(expr : Expr) -> Vec<Expr> {
  let mut const_exprs = vec![];
  find_const_exprs(&mut const_exprs, expr);
  const_exprs
}

fn find_const_exprs(const_exprs : &mut Vec<Expr>, expr : Expr) {
  match expr.shape() {
    List(ConstExpr, &[c]) => {
      const_exprs.push(c);
      return;
    }
    _ => (),
  }
  for &c in expr.children() {
    find_const_exprs(const_exprs, c);
  }
}
use std::collections::{HashMap, HashSet};

use crate::{env::{CellId, CellUid}, parse::{Expr, ExprShape, ExprTag}, perm_alloc::Ptr, symbols::Symbol};

use ExprTag::*;
use ExprShape::*;
use ReferenceType::*;

#[derive(Copy, Clone, PartialEq)]
pub enum ReferenceType {
  GlobalDef, Local,
}

/// uses expr pointers as unique keys in a map
#[derive(Clone)]
pub struct ExprMap<T> {
  map : HashMap<u64, T>,
}

impl <T> ExprMap<T> {
  fn new() -> Self {
    ExprMap { map: HashMap::new() }
  }

  pub fn get(&self, e : Expr) -> Option<&T> {
    self.map.get(&Ptr::to_u64(e))
  }

  pub fn insert(&mut self, e : Expr, v : T) {
    let id = Ptr::to_u64(e);
    if self.map.contains_key(&id) {
      panic!("multiple instances of expression encountered")
    }
    self.map.insert(id, v);
  }
}

#[derive(Clone)]
pub struct ReferenceInfo {
  pub references : ExprMap<ReferenceType>,
  pub dependencies : HashSet<CellId>,
}

pub struct ResolvedReferenceInfo {
  pub references : ExprMap<ReferenceType>,
  pub dependencies : HashMap<CellId, CellUid>,
}

pub fn get_semantic_info(expr : Expr) -> ReferenceInfo {
  let mut info = ReferenceInfo {
    references: ExprMap::new(),
    dependencies: HashSet::new(),
  };
  let mut locals = vec![];
  find_refs(&mut info, &mut locals, expr);
  info
}

fn find_refs(
  info : &mut ReferenceInfo,
  locals : &mut Vec<Symbol>,
  expr : Expr
)
{
  match expr.shape() {
    Sym(name) => {
      if !expr.metadata.ignore_symbol {
        if locals.iter().find(|&&n| n == name).is_some() {
          info.references.insert(expr, Local);
        }
        else {
          info.references.insert(expr, GlobalDef);
          let cell = CellId::DefCell(name);
          info.dependencies.insert(cell);
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
        find_refs(info, &mut function_locals, c);
      }
      return;
    }
    List(Do, exprs) => {
      let local_count = locals.len();
      for &c in exprs {
        find_refs(info, locals, c);
      }
      locals.drain(local_count..);
      return;
    }
    List(ConstExpr, &[c]) => {
      info.dependencies.insert(CellId::ExprCell(c));
      return;
    }
    List(Def, _) => {
      return;
    }
    _ => (),
  }
  for &c in expr.children() {
    find_refs(info, locals, c);
  }
}

/// All of the const expressions in the expr, and nested const exprs, in a valid order for evaluation
pub fn get_ordered_const_exprs(expr : Expr) -> Vec<Expr> {
  let mut const_exprs = vec![];
  find_const_exprs(&mut const_exprs, expr);
  const_exprs
}

fn find_const_exprs(const_exprs : &mut Vec<Expr>, expr : Expr) {
  match expr.shape() {
    List(Def, _) => {
      // Defs must be evalated in a different environment
      return;
    }
    List(ConstExpr, &[c]) => {
      // insert nested const exprs before inserting this one
      find_const_exprs(const_exprs, c);
      const_exprs.push(c);
      return;
    }
    _ => (),
  }
  for &c in expr.children() {
    find_const_exprs(const_exprs, c);
  }
}
use crate::{env::CellUid, parse::{self, Expr, ExprTag, ExprShape}, symbols::{to_symbol, Symbol, SymbolTable}, perm_alloc::{perm_slice_from_vec, perm_slice}};

pub struct CellDependencies {
  pub defs : Vec<Expr>,
  pub const_exprs : Vec<Expr>,
  pub def_refs : Vec<CellUid>,
  pub observer_refs : Vec<CellUid>,
  pub embeds : Vec<Expr>,
}

pub fn get_cell_dependencies(st : SymbolTable, expr : Expr) -> CellDependencies {
  fn find_nested_cells(
    st : SymbolTable,
    locals : &mut Vec<Symbol>,
    deps : &mut CellDependencies,
    expr : Expr
  )
  {
    use parse::ExprShape::*;
    match expr.shape() {
      Sym(name) => {
        if expr.metadata.ignore_symbol {
          return;
        }
        if locals.iter().find(|&l| *l == name).is_none() {
          let uid = expr_to_uid(expr).unwrap();
          deps.def_refs.push(uid);
        }
      }
      List(ExprTag::Observe, &[e]) => {
        if let Some(uid) = expr_to_uid(e) {
          deps.observer_refs.push(uid);
        }
      }
      List(ExprTag::Namespace, _) => {
        if let Some(uid) = expr_to_uid(expr) {
          deps.def_refs.push(uid);
        }
      }
      List(ExprTag::Let, &[name, value]) => {
        find_nested_cells(st, locals, deps, value);
        locals.push(name.as_symbol());
      }
      List(ExprTag::Def, _) | List(ExprTag::Reactive, _) => {
        deps.defs.push(expr);
      }
      List(ExprTag::ConstExpr, &[c]) => {
        deps.const_exprs.push(c);
      }
      List(ExprTag::Embed, &[e]) => {
        deps.embeds.push(e);
      }
      List(ExprTag::Quote, _) => {
        // expression literals should be ignored!
      }
      List(ExprTag::Do, exprs) => {
        let num_locals = locals.len();
        for &c in exprs {
          find_nested_cells(st, locals, deps, c);
        }
        locals.drain(num_locals..).for_each(|_| ());
      }
      List(ExprTag::Fun, &[args, ret, body]) => {
        find_nested_cells(st, locals, deps, ret);
        let mut new_locals = vec![];
        for a in args.children() {
          if let Some(&[name, tag]) = a.as_syntax() {
            new_locals.push(name.as_symbol());
            find_nested_cells(st, locals, deps, tag);
          }
        }
        find_nested_cells(st, &mut new_locals, deps, body);
      }
      List(ExprTag::Container, exprs) => {
        let TODO = (); // temporary hack to get around compiler macro issue
        let sym = to_symbol(st, "create_container");
        let uid = CellUid::DefCell(perm_slice(&[]), sym);
        deps.def_refs.push(uid);
        for &c in exprs {
          find_nested_cells(st, locals, deps, c);
        }
      }
      List(ExprTag::Stream, exprs) => {
        let TODO = (); // temporary hack to get around compiler macro issue
        let sym = to_symbol(st, "create_stream");
        let uid = CellUid::DefCell(perm_slice(&[]), sym);
        deps.def_refs.push(uid);
        for &c in exprs {
          find_nested_cells(st, locals, deps, c);
        }
      }
      _ => {
        for &c in expr.children() {
          find_nested_cells(st, locals, deps, c);
        }
      },
    }
  }

  let mut deps = CellDependencies {
    defs: vec![],
    const_exprs: vec![],
    def_refs: vec![],
    observer_refs: vec![],
    embeds: vec![]
  };
  let mut locals = vec![];
  find_nested_cells(st, &mut locals, &mut deps, expr);
  deps
}

pub fn expr_to_uid(e : Expr) -> Option<CellUid> {
  fn inner(mut names : Vec<Symbol>, e : Expr) -> Option<CellUid> {
    use ExprShape::*;
    match e.shape() {
      Sym(name) => {
        Some(CellUid::def(perm_slice_from_vec(names), name))
      }
      List(ExprTag::Namespace, &[name, tail]) => {
        names.push(name.as_symbol());
        inner(names, tail)
      },
      _ => {
        None
      }
    }
  }
  inner(vec![], e)
}

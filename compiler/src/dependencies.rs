use crate::{env::{CellIdentifier}, parse::{self, Expr, ExprTag, ExprShape}, perm_alloc::{perm_slice_from_vec, perm_slice}, symbols::{to_symbol, Symbol, SymbolTable}};

pub struct CellDependencies {
  pub inner_cells : Vec<Expr>,
  pub ids : Vec<CellIdentifier>,
  pub embeds : Vec<Expr>,
  pub observe_id : Option<CellIdentifier>,
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
          let id = expr_to_id(expr).unwrap();
          deps.ids.push(id);
        }
      }
      List(ExprTag::Observe, &[e]) => {
        if let Some(id) = expr_to_id(e) {
          deps.observe_id = Some(id);
        }
      }
      List(ExprTag::Namespace, _) => {
        if let Some(id) = expr_to_id(expr) {
          deps.ids.push(id);
        }
      }
      List(ExprTag::Let, &[name, value]) => {
        find_nested_cells(st, locals, deps, value);
        locals.push(name.as_symbol());
      }
      List(ExprTag::Def, _) | List(ExprTag::Reactive, _) => {
        deps.inner_cells.push(expr);
      }
      List(ExprTag::Cells, cell_exprs) => {
        for &ce in cell_exprs {
          deps.inner_cells.push(ce);
        }
      }
      List(ExprTag::ConstExpr, &[c]) => {
        deps.inner_cells.push(c);
        deps.ids.push(CellIdentifier::expr(c))
      }
      List(ExprTag::Embed, &[inner]) => {
        deps.embeds.push(inner);
        deps.ids.push(CellIdentifier::expr(inner))
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
        // hack to get around compiler macro issue;
        // dependencies are detected before the compiler
        // macro inserts this symbol
        let sym = to_symbol(st, "create_container");
        let id = CellIdentifier::DefCell(perm_slice(&[]), sym);
        deps.ids.push(id);
        for &c in exprs {
          find_nested_cells(st, locals, deps, c);
        }
      }
      List(ExprTag::Stream, exprs) => {
        // hack to get around compiler macro issue;
        // dependencies are detected before the compiler
        // macro inserts this symbol
        let sym = to_symbol(st, "create_stream");
        let id = CellIdentifier::DefCell(perm_slice(&[]), sym);
        deps.ids.push(id);
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
    ids: vec![],
    inner_cells: vec![],
    embeds: vec![],
    observe_id: None,
  };
  let mut locals = vec![];
  find_nested_cells(st, &mut locals, &mut deps, expr);
  deps
}

pub fn expr_to_id(e : Expr) -> Option<CellIdentifier> {
  fn inner(mut names : Vec<Symbol>, e : Expr) -> Option<CellIdentifier> {
    use ExprShape::*;
    match e.shape() {
      Sym(name) => {
        Some(CellIdentifier::def(perm_slice_from_vec(names), name))
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

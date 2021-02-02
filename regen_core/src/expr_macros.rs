
use crate::{parse::{self, Expr, ExprShape}, perm_alloc, symbols};
use parse::{ExprContent, ExprData, ExprTag, SrcLocation};
use perm_alloc::{perm, perm_slice_from_vec};
use symbols::{SymbolTable};

pub fn template(e : Expr, args : &[Expr]) -> Expr {
  pub fn template_inner(e : Expr, args : &[Expr], next_arg : &mut usize) -> Expr {
    use ExprShape::*;
    match e.shape() {
      Ref(_) | Literal(_) => e,
      List(ExprTag::TemplateHole, &[_]) => {
        let new_e = args[*next_arg];
        *next_arg += 1;
        new_e
      }
      List(_, cs) => {
        let mut children = vec![];
        for &c in cs {
          children.push(template_inner(c, args, next_arg));
        }
        let ed = ExprData {
          tag: e.tag,
          content: ExprContent::List(perm_slice_from_vec(children)),
          loc: e.loc,
        };
        perm(ed)
      },
    }
  }
  template_inner(e, args, &mut 0)
}

pub struct ExprBuilder {
  pub loc : SrcLocation,
  pub st : SymbolTable,
}
  
impl ExprBuilder {

  fn set_loc(&self, mut e : Expr) {
    e.loc = self.loc;
    for &c in e.children() { self.set_loc(c) }
  }

  fn sexp(&self, s : &str) -> Expr {
    let e = parse::parse_expression(self.st, &self.loc.module.name, s).unwrap();
    self.set_loc(e);
    e
  }

  fn expr(&self, tag : ExprTag, content : ExprContent) -> Expr {
    perm(ExprData { tag, content, loc: self.loc })
  }
}

pub fn template_macro(nb : &ExprBuilder, e : Expr, args : Vec<Expr>) -> Expr {
  // (do
  //   (let args (array a b c))
  //   (template_quote e (ref args) (array_len args))
  // )
  let array = {
    let c = ExprContent::List(perm_slice_from_vec(args));
    nb.expr(ExprTag::ArrayInit, c)
  };
  let template_call = nb.sexp("
    (do
      (let args ($ array)
      (template_quote
        (quote ($ e))
        (cast (ref args) (ptr node))
        (array_len args))
    )
  ");
  template(template_call, &[array, e])
}

pub fn for_macro(nb : &ExprBuilder, loop_var : Expr, start : Expr, end : Expr, body : Expr) -> Expr {
  let loop_template = nb.sexp("
    (do
      (let ($ loop_var) ($ start))
      (let _end ($ end))
      (label for_loop (do
        (if (< ($ loop_var) _end) (do
          ($ body)
          (set ($ loop_var) (+ ($ loop_var) 1))
          repeat
        ))
      ))
    )
  ");
  template(loop_template, &[loop_var, start, end, loop_var, body, loop_var, loop_var])
}

pub fn while_macro(nb : &ExprBuilder, cond : Expr, body : Expr) -> Expr {
  let loop_template = nb.sexp("
    (label while_loop (do
      (if ($ cond) (do
        ($ body)
        repeat
      ))
    ))
  ");
  template(loop_template, &[cond, body])
}

pub fn slice_index_macro(nb : &ExprBuilder, slice : Expr, index : Expr) -> Expr {
  let slice_index_template =
    nb.sexp("(ptr_index (. ($ slice) data) ($ index))");
  template(slice_index_template, &[slice, index])
}

pub fn slice_type_macro(nb : &ExprBuilder, element_type : Expr) -> Expr {
  let slice_type_template =
    nb.sexp("(struct (data (ptr ($ element_type)) (len u64))");
  template(slice_type_template, &[element_type])
}

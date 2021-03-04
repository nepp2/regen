
use crate::{parse::{self, Expr, ExprShape}, perm_alloc, symbols};
use parse::{ExprContent, ExprData, ExprMetadata, ExprTag, SrcLocation};
use perm_alloc::{perm, perm_slice_from_vec};
use symbols::{SymbolTable};

pub fn template(e : Expr, args : &[Expr]) -> Expr {
  pub fn template_inner(e : Expr, args : &[Expr], next_arg : &mut usize) -> Expr {
    use ExprShape::*;
    match e.shape() {
      Sym(_) | Literal(_) => e,
      List(ExprTag::TemplateHole, &[_]) => {
        let mut new_e = args[*next_arg].deep_clone();
        *next_arg += 1;
        // preserve semantic information from the template expression
        new_e.metadata.ignore_symbol = e.metadata.ignore_symbol;
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
          metadata: e.metadata,
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
    e.metadata.loc = self.loc;
    for &c in e.children() { self.set_loc(c) }
  }

  fn parse(&self, s : &str) -> Expr {
    let e = parse::parse_expression(self.st, &self.loc.module.name, s).unwrap();
    self.set_loc(e);
    e
  }

  fn expr(&self, tag : ExprTag, content : ExprContent) -> Expr {
    perm(ExprData {
      tag,
      content,
      metadata: ExprMetadata {
        loc: self.loc,
        ignore_symbol: false 
      }
    })
  }
}

pub fn struct_type_macro(nb : &ExprBuilder, names : Vec<Expr>, types : Vec<Expr>) -> Expr {
  let names = {
    let c = ExprContent::List(perm_slice_from_vec(names));
    nb.expr(ExprTag::ArrayInit, c)
  };
  let types = {
    let c = ExprContent::List(perm_slice_from_vec(types));
    nb.expr(ExprTag::ArrayInit, c)
  };
  let tstr = nb.parse("
    {
      let names = $names;
      let types = $types;
      struct_type(
        array_len(names),
        (ref names) as ptr_type(symbol),
        (ref types) as ptr_type(type),
      )
    }
  ");
  template(tstr, &[names, types])
}

pub fn fn_type_macro(nb : &ExprBuilder, arg_types : Vec<Expr>, ret : Expr, is_cfun : bool) -> Expr {
  let array = {
    let c = ExprContent::List(perm_slice_from_vec(arg_types));
    nb.expr(ExprTag::ArrayInit, c)
  };
  let fn_type_call = nb.parse("
    {
      let args = $array;
      fun_type(
        (ref args) as ptr_type(type),
        array_len(args),
        $ret,
        false,
      )
    }
  ");
  let cfun_type_call = nb.parse("
    {
      let args = $array;
      fun_type(
        (ref args) as ptr_type(type),
        array_len(args),
        $ret,
        true,
      )
    }
  ");
  if is_cfun {
    template(cfun_type_call, &[array, ret])
  }
  else {
    template(fn_type_call, &[array, ret])
  }
}

pub fn template_macro(nb : &ExprBuilder, e : Expr, args : Vec<Expr>) -> Expr {
  let array = {
    let c = ExprContent::List(perm_slice_from_vec(args));
    nb.expr(ExprTag::ArrayInit, c)
  };
  let template_call = nb.parse("
    {
      let args = $array;
      template_quote(quote $e,
        (ref args) as ptr expr,
        array_len(args))
    }
  ");
  template(template_call, &[array, e])
}

pub fn for_macro(nb : &ExprBuilder, loop_var : Expr, start : Expr, end : Expr, body : Expr) -> Expr {
  let loop_template = nb.parse("
    {
      let $loop_var = $start;
      let _end = $end;
      label for_loop {
        if $loop_var < _end {
          $body;
          $loop_var = $loop_var + 1;
          repeat;
        }
      }
    }
  ");
  template(loop_template, &[loop_var, start, end, loop_var, body, loop_var, loop_var])
}

pub fn while_macro(nb : &ExprBuilder, cond : Expr, body : Expr) -> Expr {
  let loop_template = nb.parse("
    label while_loop {
      if $cond {
        $body;
        repeat;
      }
    }
  ");
  template(loop_template, &[cond, body])
}

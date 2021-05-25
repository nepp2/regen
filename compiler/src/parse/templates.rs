
use std::vec;

use crate::{parse::{self, Expr, ExprShape}, regen_alloc, symbols::{self, to_symbol}};
use parse::{ExprContent, ExprData, ExprMetadata, ExprTag, SrcLocation};
use regen_alloc::{alloc, alloc_slice};
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
          content: ExprContent::List(alloc_slice(children)),
          metadata: e.metadata,
        };
        alloc(ed)
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

  pub fn new(loc : SrcLocation, st : SymbolTable) -> Self {
    Self { loc, st }
  }

  fn set_loc(&self, mut e : Expr) {
    e.metadata.loc = self.loc;
    for &c in e.children() { self.set_loc(c) }
  }

  fn parse(&self, s : &str) -> Expr {
    let e = parse::parse_expression(self.st, self.loc.module.name, s).unwrap();
    self.set_loc(e);
    e
  }

  fn array_expr(&self, type_name : &str, list : Vec<Expr>) -> Expr {
    let sym = to_symbol(self.st, type_name);
    let tag = self.expr(ExprTag::Name, ExprContent::Sym(sym));
    let const_tag = self.list_expr(ExprTag::ConstExpr, vec![tag]);
    let elements = self.list_expr(ExprTag::Structural, list);
    self.list_expr(ExprTag::ArrayInit, vec![const_tag, elements])
  }


  fn list_expr(&self, tag : ExprTag, list : Vec<Expr>) -> Expr {
    self.expr(tag, ExprContent::List(alloc_slice(list)))
  }

  fn expr(&self, tag : ExprTag, content : ExprContent) -> Expr {
    alloc(ExprData {
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
  let names = nb.array_expr("symbol", names);
  let types = nb.array_expr("Type", types);
  let tstr = nb.parse("
    {
      let names = $names;
      let types = $types;
      struct_type(
        array_len(names),
        (ref names) as ptr_type(symbol),
        (ref types) as ptr_type(Type),
      )
    }
  ");
  template(tstr, &[names, types])
}

pub fn fn_type_macro(nb : &ExprBuilder, arg_types : Vec<Expr>, ret : Expr, is_cfun : bool) -> Expr {
  let array = nb.array_expr("Type", arg_types);
  let fn_type_call = nb.parse("
    {
      let args = $array;
      fun_type(
        (ref args) as ptr_type(Type),
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
        (ref args) as ptr_type(Type),
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

pub fn named_type_macro(nb : &ExprBuilder, name : Expr, type_expr : Expr) -> Expr {
  let template_call = nb.parse("named_type(sym $name, $type_expr)");
  template(template_call, &[name, type_expr])
}

pub fn template_macro(nb : &ExprBuilder, e : Expr, args : Vec<Expr>) -> Expr {
  let array = nb.array_expr("expr", args);
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

pub fn create_container_ref(nb : &ExprBuilder) -> Expr {
  nb.parse("create_container")
}

pub fn create_stream_ref(nb : &ExprBuilder) -> Expr {
  nb.parse("create_stream")
}

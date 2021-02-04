
use crate::{bytecode, error::Error, expr_macros, perm_alloc, symbols};
use super::{
  sexp::{
    self,
    Node,
    NodeLiteral,
    NodeShape::*,
    NodeContent,
  },
  expr::*,
};
use expr_macros::{ExprBuilder, template_macro};
use symbols::SymbolTable;
use perm_alloc::{SlicePtr, perm, perm_slice, perm_slice_from_vec};
use bytecode::Operator;


use ExprTag::*;
use ExprContent::*;

pub fn parse_module(st : SymbolTable, module_name : &str, code : &str) -> Result<Vec<Expr>, Error> {
  let root = sexp::sexp_list(st, module_name, code);
  let mut es = vec![];
  for &n in root.children() {
    es.push(parse_expr(st, n));
  }
  Ok(es)
}

pub fn parse_expression(st : SymbolTable, module_name : &str, code : &str) -> Result<Expr, Error> {
  let n = sexp::sexp(st, module_name, code);
  Ok(parse_expr(st, n))
}

fn list_expr(n : Node, tag : ExprTag, exprs : &[Expr]) -> Expr {
  expr(n.loc, tag, List(perm_slice(exprs)))
}

fn list_expr_from_vec(n : Node, tag : ExprTag, exprs : Vec<Expr>) -> Expr {
  expr(n.loc, tag, List(perm_slice_from_vec(exprs)))
}

fn literal_expr(n : Node, v : Val) -> Expr {
  expr(n.loc, LiteralExpr, LiteralVal(v))
}

fn symbol_literal(n : Node) -> Expr {
  literal_expr(n, Val::Symbol(n.as_symbol()))
}

fn expr(loc : SrcLocation, tag : ExprTag, content: ExprContent) -> Expr {
  perm(ExprData { tag, content, loc })
}

fn parse_expr(st : SymbolTable, n : Node) -> Expr {
  match sexp::node_shape(&n) {
    // symbol reference (local var or global def)
    Atom(s) => {
      match s {
        "return" => list_expr(n, Return, &[]),
        "break" => list_expr(n, Break, &[]),
        "repeat" => list_expr(n, Repeat, &[]),
        "true" => literal_expr(n, Val::Bool(true)),
        "false" => literal_expr(n, Val::Bool(true)),
        _ => {
          let sym = n.as_symbol();
          expr(n.loc, Reference, LiteralVal(Val::Symbol(sym)))
        }
      }
    }
    // literal
    Literal(l) => {
      match l {
        NodeLiteral::I64(v) => {
          literal_expr(n, Val::I64(v))
        }
        NodeLiteral::String(s) => {
          literal_expr(n, Val::String(s))
        }
      }
    }
    // break to label
    Command("break", &[label]) => {
      let label_expr = symbol_literal(label);
      list_expr(n, Break, &[label_expr])
    }
    // repeat to label
    Command("repeat", &[label]) => {
      let label_expr = symbol_literal(label);
      list_expr(n, Repeat, &[label_expr])
    }
    // array
    Command("array", elements) => {
      let es = parse_expr_list(st, elements);
      expr(n.loc, ArrayInit, List(es))
    }
    // array length
    Command("array_len", &[e]) => {
      list_expr(n, ArrayLen, &[parse_expr(st, e)])
    }
    // array to slice
    Command("as_slice", &[e]) => {
      list_expr(n, ArrayAsSlice, &[parse_expr(st, e)])
    }
    // array index
    Command("index", &[v, index]) => {
      list_expr(n, ArrayIndex, &[
        parse_expr(st, v), // array
        parse_expr(st, index), // index
      ])
    }
    // ptr index
    Command("ptr_index", &[v, index]) => {
      list_expr(n, PtrIndex, &[
        parse_expr(st, v), // pointer
        parse_expr(st, index), // index
      ])
    }
    // slice type
    Command("slice_index", &[v, index]) => {
      let nb = ExprBuilder { loc: n.loc, st };
      expr_macros::slice_index_macro(&nb,
        parse_expr(st, v),
        parse_expr(st, index),
      )
    }
    // init
    Command("zero_init", &[type_node]) => {
      list_expr(n, ZeroInit, &[parse_expr(st, type_node)])
    }
    // init
    Command("init", ns) => {
      let type_value = parse_expr(st, ns[0]);
      let mut v = vec![type_value];
      parse_to_vec(st, &ns[1..], &mut v);
      list_expr_from_vec(n, StructInit, v)
    }
    // field deref
    Command(".", &[v, field]) => {
      let struct_val = parse_expr(st, v);
      let field_name = symbol_literal(field);
      list_expr(n, FieldIndex, &[struct_val, field_name])
    }
    // template
    Command("#", &[quoted]) => {
      to_template_expr(st, parse_expr(st, quoted))
    }
    // template hole
    Command("$", &[quoted]) => {
      let e = parse_expr(st, quoted);
      list_expr(n, TemplateHole, &[e])
    }
    // for
    Command("for", &[loop_var, start, end, body]) => {
      let nb = ExprBuilder { loc: n.loc, st };
      expr_macros::for_macro(&nb,
        parse_expr(st, loop_var),
        parse_expr(st, start),
        parse_expr(st, end),
        parse_expr(st, body)
      )
    }
    // while
    Command("while", &[cond, body]) => {
      let nb = ExprBuilder { loc: n.loc, st };
      expr_macros::while_macro(&nb,
        parse_expr(st, cond),
        parse_expr(st, body),
      )
    }
    // quotation
    Command("quote", &[quoted]) => {
      let e = parse_expr(st, quoted);
      literal_expr(n, Val::Expr(e))
    }
    // fun
    Command("fun", &[arg_nodes, body]) => {
      let args = parse_function_args(st, arg_nodes);
      let body = parse_expr(st, body);
      let ret = list_expr(n, Omitted, &[]);
      list_expr(n, Fun, &[args, ret, body])
    }
    Command("fun", &[arg_nodes, return_tag, body]) => {
      let args = parse_function_args(st, arg_nodes);
      let body = parse_expr(st, body);
      let ret = parse_expr(st, return_tag);
      list_expr(n, Fun, &[args, ret, body])
    }
    // set var
    Command("set", &[dest, value]) => {
      let dest = parse_expr(st, dest);
      let value = parse_expr(st, value);
      list_expr(n, Set, &[dest, value])
    }
    // let
    Command("let", &[var_name, value]) => {
      let name = symbol_literal(var_name);
      let val_expr = parse_expr(st, value);
      list_expr(n, Let, &[name, val_expr])
    }
    // def
    Command("def", &[name, initialiser]) => {
      let name = symbol_literal(name);
      let init_expr = parse_expr(st, initialiser);
      list_expr(n, Def, &[name, init_expr])
    }
    // if then
    Command("if", &[cond_node, then_expr]) => {
      list_expr(n, IfElse, &[
        parse_expr(st, cond_node),
        parse_expr(st, then_expr),
      ])
    }
    // if then else
    Command("if", &[cond_node, then_expr, else_expr]) => {
      list_expr(n, IfElse, &[
        parse_expr(st, cond_node),
        parse_expr(st, then_expr),
        parse_expr(st, else_expr),
      ])
    }
    // label expression
    Command("label", &[label, body]) => {
      list_expr(n, LabelledBlock, &[
        symbol_literal(label),
        parse_expr(st, body),
      ])
    }
    Command("do", exprs) => {
      let es = parse_expr_list(st, exprs);
      expr(n.loc, Do, List(es))
    }
  // Debug
    Command("debug", &[v]) => {
      list_expr(n, Debug, &[parse_expr(st, v)])
    }
    // Return
    Command("return", &[v]) => {
      list_expr(n, Return, &[parse_expr(st, v)])
    }
    // symbol
    Command("sym", &[v]) => {
      symbol_literal(v)
    }
    // typeof
    Command("typeof", &[v]) => {
      list_expr(n, TypeOf, &[parse_expr(st, v)])
    }
    // load
    Command("*", &[pointer]) => {
      list_expr(n, Deref, &[parse_expr(st, pointer)])
    }
    // ref
    Command("ref", &[locator]) => {
      list_expr(n, GetAddress, &[parse_expr(st, locator)])
    }
    // cast
    Command("cast", &[value, type_tag]) => {
      list_expr(n, Cast, &[
        parse_expr(st, value),
        parse_expr(st, type_tag),
      ])
    }
    // pointer type
    Command("ptr", &[inner_type]) => {
      list_expr(n, PtrType, &[parse_expr(st, inner_type)])
    }
    // function type
    Command("fn", &[args, ret]) => {
      list_expr(n, FnType, &[
        parse_function_args(st, args),
        parse_expr(st, ret),
      ])
    }
    // c function type
    Command("cfun", &[args, ret]) => {
      list_expr(n, CFunType, &[
        parse_function_args(st, args),
        parse_expr(st, ret),
      ])
    }
    // array type
    Command("sized_array", &[element, length]) => {
      list_expr(n, SizedArrayType, &[
        parse_expr(st, element),
        parse_expr(st, length),
      ])
    }
    // struct type
    Command("struct", fields) => {
      let fs = to_field_list(st, fields);
      expr(n.loc, StructType, List(fs))
    }
    // slice type
    Command("slice", &[element_type]) => {
      let nb = ExprBuilder { loc: n.loc, st };
      expr_macros::slice_type_macro(&nb,
        parse_expr(st, element_type))
    }
    _ => {
      let ns = n.children();
      if ns.len() == 0 {
        return literal_expr(n, Val::Void);
      }
      if let [lit, tag] = ns {
        if let NodeContent::Literal(NodeLiteral::I64(_)) = lit.content {
          return list_expr(n, Cast, &[
            parse_expr(st, *lit),
            parse_expr(st, *tag),
          ]);
        }
      }
      if let NodeContent::Sym(sym) = ns[0].content {
        if let Some(op) = str_to_operator(sym.as_str()) {
          let op = literal_expr(ns[0], Val::Operator(op));
          let mut es = vec![op];
          parse_to_vec(st, &ns[1..], &mut es);
          return list_expr_from_vec(n, InstrinicOp, es);
        }
      }
      let es = parse_expr_list(st, ns);
      expr(n.loc, Call, List(es))
    }
  }
}

fn parse_function_args(st : SymbolTable, args_node : Node) -> Expr {
  let mut v = Vec::with_capacity(args_node.children().len());
  for &a in args_node.children() {
    if let [name, type_tag] = a.children() {
      let name_literal = symbol_literal(*name);
      let type_expr = parse_expr(st, *type_tag);
      let arg = list_expr(a, Syntax, &[name_literal, type_expr]);
      v.push(arg);
    }
    else {
      panic!()
    }
  }
  let args = perm_slice_from_vec(v);
  expr(args_node.loc, Syntax, List(args))
}

fn to_field_list(st : SymbolTable, fields : &[Node]) -> SlicePtr<Expr> {
  let mut v = Vec::with_capacity(fields.len());
  for f in fields {
    let field = {
      if let [name, type_tag] = f.children() {
        list_expr(*f, Syntax, &[
          symbol_literal(*name),
          parse_expr(st, *type_tag),
        ])
      }
      else {
        panic!("expected field at ({})", f.loc)
      }
    };
    v.push(field);
  }
  perm_slice_from_vec(v)
}

fn parse_expr_list(st : SymbolTable, ns : &[Node]) -> SlicePtr<Expr> {
  let mut v = Vec::with_capacity(ns.len());
  parse_to_vec(st, ns, &mut v);
  perm_slice_from_vec(v)
}

fn parse_to_vec(st : SymbolTable, ns : &[Node], v : &mut Vec<Expr>) {
  for &n in ns {
    v.push(parse_expr(st, n));
  }
}

fn str_to_operator(s : &str) -> Option<Operator> {
  use Operator::*;
  let op = match s {
    "+" => Add,
    "-" => Sub,
    "*" => Mul,
    "/" => Div,
    "%" => Rem,
    "==" => Eq,
    "!=" => NEq,
    "<" => LT,
    ">" => GT,
    "<=" => LTE,
    ">=" => GTE,
    "!" => Not,
    _ => return None,
  };
  Some(op)
}

fn to_template_expr(st : SymbolTable, quoted : Expr) -> Expr {
  
  fn find_template_arguments(e : Expr, args : &mut Vec<Expr>) {
    match e.shape() {
      ExprShape::List(TemplateHole, &[inner]) => {
        args.push(inner);
      }
      _ => (),
    }
    for &c in e.children() {
      find_template_arguments(c, args);
    }
  }

  let mut template_args = vec![];
  find_template_arguments(quoted, &mut template_args);
  if template_args.len() > 0 {
    let nb = ExprBuilder { loc: quoted.loc, st };
    template_macro(&nb, quoted, template_args)
  }
  else {
    expr(quoted.loc, ExprTag::LiteralExpr, ExprContent::LiteralVal(Val::Expr(quoted)))
  }
}

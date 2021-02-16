use std::hash::{Hash, Hasher};

use crate::{bytecode::Operator, ffi_libs::RegenString, perm_alloc::{Ptr, SlicePtr, perm, perm_slice_from_vec}, symbols::Symbol};

#[derive(Clone)]
pub struct CodeModule {
  pub name : String,
  pub code : String,
}

#[derive(Clone, Copy)]
pub struct SrcLocation {
  pub start : usize,
  pub end : usize,  
  pub module : Ptr<CodeModule>,
}

pub type Expr = Ptr<ExprData>;

#[derive(Copy, Clone)]
pub struct ExprData {
  pub tag : ExprTag,
  pub content : ExprContent,
  pub loc : SrcLocation,

  /// Indicates that, if this is a symbol, it is not a local or global reference.
  /// It may be something like a field name or a new variable definition.
  pub ignore_symbol : bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExprTag {
  Def,
  Let,
  Name,
  StructInit,
  ZeroInit,
  ArrayInit,
  ArrayIndex,
  ArrayAsSlice,
  PtrIndex,
  FieldIndex,
  LiteralExpr,
  Call,
  InstrinicOp,
  Cast,
  IfElse,
  Debug,
  Set,
  Deref,
  GetAddress,
  Return,
  Break,
  Repeat,
  LabelledBlock,
  Do,
  Fun,
  ArrayLen,
  TypeOf,
  FnType,
  CFunType,
  StructType,
  PtrType,
  SizedArrayType,
  TemplateHole,
  Omitted, // was not specified
  Syntax, // cannot be evaluated
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Val {
  I64(i64),
  F64(u64),
  String(Ptr<RegenString>),
  Bool(bool),
  Expr(Expr),
  Symbol(Symbol),
  Void,
  Operator(Operator),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExprContent {
  List(SlicePtr<Expr>),
  Sym(Symbol),
  LiteralVal(Val),
}

use ExprTag::*;
use ExprContent::*;

impl ExprData {
  pub fn shape(&self) -> ExprShape {
    match self.content {
      List(l) => ExprShape::List(self.tag, l.as_slice()),
      Sym(s) => ExprShape::Ref(s),
      LiteralVal(v) => ExprShape::Literal(v),
    }
  }

  pub fn children(&self) -> &[Expr] {
    if let List(es) = self.content {
      return es.as_slice();
    }
    &[]
  }

  pub fn as_syntax(&self) -> Option<&[Expr]> {
    if self.tag == Syntax {
      if let List(es) = self.content {
        return Some(es.as_slice());
      }
    }
    None
  }

  pub fn as_symbol(&self) -> Symbol {
    if let Sym(s) = self.content {
      return s;
    }
    panic!("expected symbol")
  }

  pub fn as_expr_literal(&self) -> Expr {
    if let LiteralVal(Val::Expr(e)) = self.content {
      return e;
    }
    panic!("expected expr")
  }

  pub fn as_operator_literal(&self) -> Operator {
    if let LiteralVal(Val::Operator(op)) = self.content {
      return op;
    }
    panic!("expected operator")
  }

  pub fn deep_clone(&self) -> Expr {
    let content = match self.content {
      List(es) => {
        let mut children = vec![];
        for e in es {
          children.push(e.deep_clone());
        }
        List(perm_slice_from_vec(children))
      }
      Sym(s) => Sym(s),
      LiteralVal(v) => LiteralVal(v),
    };
    let ed = ExprData {
      tag: self.tag,
      content,
      loc: self.loc,
      ignore_symbol: self.ignore_symbol,
    };
    perm(ed)
  }
}

impl PartialEq for ExprData {
  fn eq(&self, rhs : &Self) -> bool {
    self.tag == rhs.tag && self.content == rhs.content
  }
}
impl Eq for ExprData {}
impl Hash for ExprData {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.tag.hash(state);
    self.content.hash(state);
  } 
}

pub enum ExprShape<'l> {
  List(ExprTag, &'l [Expr]),
  Literal(Val),
  Ref(Symbol),
}

use std::fmt;

impl SrcLocation {
  pub fn zero() -> Self {
    SrcLocation {
      start: 0, end: 0, module: Ptr { p : std::ptr::null_mut() },
    }
  }

  pub fn src_snippet(&self) -> &str {
    &self.module.code.as_str()[self.start..self.end]
  }
}

impl fmt::Display for SrcLocation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut line_start = 0;
    let mut line_number = 1;
    for (i, c) in self.module.code.as_str()[0..self.start].char_indices() {
      if c == '\n' {
        line_start = i;
        line_number += 1;
      }
    }
    write!(f, "'{}', line {}, col {} to {}",
      self.module.name,
      line_number,
      self.start - line_start,
      self.end - line_start)
  }
}

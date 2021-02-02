use std::hash::{Hash, Hasher};

use crate::{
  bytecode::Operator,
  ffi_libs::RegenString,
  perm_alloc::{Ptr, SlicePtr},
  symbols::Symbol,
};

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
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExprTag {
  Def,
  Let,
  Reference,
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
  Implicit, // was not specified
  Syntax, // cannot be evaluated
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Val {
  I64(i64), // TODO: rename
  F64(u64),
  String(Ptr<RegenString>),
  Bool(bool),
  Symbol(Symbol),
  Expr(Expr),
  Void,
  Operator(Operator),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExprContent {
  List(SlicePtr<Expr>),
  LiteralVal(Val),
}

use ExprTag::*;
use ExprContent::*;

impl ExprData {
  pub fn shape(&self) -> ExprShape {
    match self.content {
      List(l) => ExprShape::List(self.tag, l.as_slice()),
      LiteralVal(v) => {
        if self.tag == ExprTag::Reference {
          if let Val::Symbol(s) = v {
            return ExprShape::Ref(s);
          }
        }
        return ExprShape::Literal(v);
      }
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

  pub fn as_symbol_literal(&self) -> Symbol {
    if let LiteralVal(Val::Symbol(s)) = self.content {
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

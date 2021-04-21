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

pub fn expr(tag : ExprTag, content : ExprContent, loc : SrcLocation) -> Expr {
  let metadata = ExprMetadata { loc, ignore_symbol : false };
  let ed = ExprData { tag, content, metadata };
  perm(ed)
}

/// Metadata is not taken into account during expr comparisons (Hash, PartialEq, etc)
#[derive(Copy, Clone)]
pub struct ExprMetadata {
  pub loc : SrcLocation,
  
  /// Indicates that, if this is a symbol, it is not a local or global reference.
  /// It may be something like a field name or a new variable definition.
  pub ignore_symbol : bool,
}

#[derive(Copy, Clone)]
pub struct ExprData {
  pub tag : ExprTag,
  pub content : ExprContent,
  pub metadata : ExprMetadata,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExprTag {
  ConstExpr,
  Def,
  Reactive,
  OnChange,
  Container,
  Stream,
  Let,
  Embed,
  Name,
  Namespace,
  StructInit,
  ZeroInit,
  ArrayInit,
  Index,
  ArrayAsSlice,
  PtrIndex,
  FieldIndex,
  LiteralSymbol,
  LiteralVal,
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
  TemplateHole,
  Quote,
  Omitted, // was not specified
  Syntax, // cannot be evaluated
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Val {
  I64(i64),
  F64(u64),
  String(Ptr<RegenString>),
  Bool(bool),
  Void,
  Operator(Operator),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExprContent {
  List(SlicePtr<Expr>),
  Sym(Symbol),
  Value(Val),
}

use ExprTag::*;
use ExprContent::*;

impl ExprData {
  pub fn shape(&self) -> ExprShape {
    match self.content {
      List(l) => ExprShape::List(self.tag, l.as_slice()),
      Sym(s) => ExprShape::Sym(s),
      Value(v) => ExprShape::Literal(v),
    }
  }

  pub fn loc(&self) -> SrcLocation { self.metadata.loc }

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
    panic!("expected symbol, found '{}'", self)
  }

  pub fn as_operator_literal(&self) -> Operator {
    if let Value(Val::Operator(op)) = self.content {
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
      Value(v) => Value(v),
    };
    let ed = ExprData {
      tag: self.tag,
      content,
      metadata: self.metadata,
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
  Sym(Symbol),
}

use std::fmt;

impl SrcLocation {
  pub fn zero(module : Ptr<CodeModule>) -> Self {
    SrcLocation {
      start: 0, end: 0, module,
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

impl fmt::Display for Val {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Val::*;
    match self {
      I64(v) => write!(f, "{}", v)?,
      F64(v) => write!(f, "{}", v)?,
      String(v) => write!(f, "{}", v.as_str())?,
      Bool(v) => write!(f, "{}", v)?,
      Void => write!(f, "void")?,
      Operator(v) => write!(f, "{}", v)?,
    }
    Ok(())
  }
}

fn write_indent(f: &mut fmt::Formatter<'_>, indent : i64) -> fmt::Result {
  for _ in 0..indent { write!(f, "  ")? }
  Ok(())
}

fn write_exprs_linebreak(es : &[Expr], f: &mut fmt::Formatter<'_>, indent : i64) -> fmt::Result {
  for c in es {
    writeln!(f)?;
    write_indent(f, indent)?;
    write_expr(&c, f, indent)?;
  }
  writeln!(f)
}

fn write_expr(e : &ExprData, f: &mut fmt::Formatter<'_>, indent : i64) -> fmt::Result {
  match e.content {
    List(es) => {
      match e.tag {
        Do => {
          write!(f, "{{")?;
          write_exprs_linebreak(es.as_slice(), f, indent + 1)?;
          write_indent(f, indent)?;
          write!(f, "}}")?;
        }
        _ => {
          write!(f, "({:?}", e.tag)?;
          for c in es {
            write!(f, " ")?;
            write_expr(&c, f, indent)?;
          }
          write!(f, ")")?;
        }
      }
    }
    Sym(s) => write!(f, "{}", s)?,
    Value(v) => write!(f, "{}", v)?,
  }
  Ok(())
}

impl fmt::Display for ExprData {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write_expr(self, f, 0)
  }
}


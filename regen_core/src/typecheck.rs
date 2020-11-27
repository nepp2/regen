
use crate::{
  parse::{Expr, ExprContent},
  types::TypeHandle,
};

#[derive(Copy, Clone)]
pub struct ExprType {
  t : TypeHandle,
  const_expr : bool,
  is_locator : bool,
  mutable : bool,
}

pub fn resolve_type(e : Expr) {
  use ExprContent::*;
  match e.content {
    Def(name, value) => {

    }
    Let(name, value) => {

    }
    LocalRef(name) => {

    }
    GlobalRef(name) => {

    }
    StructInit(type_value, field_vals) => {

    }
    ZeroInit(type_value) => {

    }
    ArrayInit(element_vals) => {

    }
    Index { array, offset } => {

    }
    FieldIndex { structure, field_name } => {

    }
    LiteralU64(v) => {

    }
    LiteralString(s) => {

    }
    LiteralBool(v) => {

    }
    LiteralVoid => {

    }
    Call(function_val, arg_vals) => {

    }
    Operator(op) => {

    }
    Cast{ value, to_type } => {

    }
    IfElse { cond, then_expr, else_expr } => {

    }
    Debug(v) => {

    }
    Sym(s) => {

    }
    Set{ dest, value } => {

    }
    Deref(ptr) => {

    }
    Ref(locator) => {

    }
    Return(v) => {

    }
    Break(to_label) => {

    }
    Repeat(to_label) => {

    }
    LabelledBlock(label, block) => {

    }
    Do(exprs) => {

    }
    Template(n) => {

    }
    Quote(n) => {

    }
    Fun { args, ret, body } => {

    }
    Macro{ arg , body } => {

    }
    ArrayLen(array) => {

    }
    TypeOf(v) => {

    }
    FnType { args, ret } => {

    }
    CFunType { args, ret } => {

    }
    StructType(fields) => {

    }
    PtrType(inner) => {

    }
    SizedArrayType{ element_type, length } => {

    }
  }
}

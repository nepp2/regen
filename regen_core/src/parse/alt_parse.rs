
use crate::{
  bytecode::Operator,
  error::error_raw,
  expr_macros,
  ffi_libs,
  perm_alloc::{Ptr, perm, perm_slice_from_vec},
  symbols::{Symbol, SymbolTable, to_symbol}
};
use super::{
  sexp::{CodeModule, SrcLocation},
  lexer::{Token, TokenType},
  parse::{Expr, ExprContent, ExprData, ExprShape, ExprTag, Val}
};
use crate::error::{Error, error};
use std::collections::{HashSet, HashMap};
use std::str::FromStr;

use ExprTag::*;

pub static EXPECTED_TOKEN_ERROR : &str = "Expected token. Found nothing.";

struct ParseConfig<'l> {
  next_precedence : i32,
  paren_pairs : HashMap<&'l str, &'l str>,
  paren_terminators : HashSet<&'l str>,
  expression_separators : HashMap<&'l str, i32>,
  prefix_precedence : HashMap<&'l str, i32>,
  infix_precedence : HashMap<&'l str, i32>,
}

impl <'l> ParseConfig<'l> {
  fn new(paren_pairs : &[(&'l str, &'l str)]) -> Self {
    let mut c = ParseConfig {
      next_precedence: 1,
      paren_pairs: HashMap::new(),
      paren_terminators: HashSet::new(),
      expression_separators: HashMap::new(),
      prefix_precedence: HashMap::new(),
      infix_precedence: HashMap::new(),
    };
    for &(a, b) in paren_pairs {
      c.paren_pairs.insert(a, b);
      c.paren_terminators.insert(b);
    }
    c
  }

  fn separator(&mut self, sep : &'l str) {
    self.expression_separators.insert(sep, self.next_precedence);
    self.next_precedence += 1;
  }

  fn infix_prefix(&mut self, infix : &[&'l str], prefix : &[&'l str]) {
    for &op in infix {
      self.infix_precedence.insert(op, self.next_precedence);
    }
    for &op in prefix {
      self.prefix_precedence.insert(op, self.next_precedence);
    }
    self.next_precedence += 1;
  }

  fn infix(&mut self, ops : &[&'l str]) {
    self.infix_prefix(ops, &[]);
  }

  fn prefix(&mut self, ops : &[&'l str]) {
    self.infix_prefix(&[], ops);
  }
}

fn parse_config() -> ParseConfig<'static> {
  let paren_pairs = &[
    ("(", ")"),
    ("{", "}"),
    ("[", "]"),
  ];
  let mut c = ParseConfig::new(paren_pairs);
  c.separator(";");
  c.separator(",");
  c.prefix(&["#keyword"]);
  c.infix(&[":"]);
  c.infix(&["as"]);
  c.infix(&["&&", "||"]);
  c.infix(&[">", "<", ">=", "<=", "==", "!="]);
  c.infix(&["%"]);
  c.infix_prefix(&["+", "-"], &["-"]);
  c.infix(&["*", "/", "%"]);
  c.infix_prefix(&["=>"], &["!", "&", "*",]);
  c.infix(&["(", "["]);
  c.infix(&["."]);
  c.prefix(&["#", "$"]);
  c
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

// TODO: this might be better implemented with a ring buffer (or just a backwards vec)
struct ParseState<'l> {
  code_module : Ptr<CodeModule>,
  tokens : Vec<Token<'l>>,
  pos : usize,
  config : &'l ParseConfig<'static>,
  st : SymbolTable,
}

use TokenType::*;
use expr_macros::{NodeBuilder, template_macro};

fn match_symbol(t : &Token, s : &str) -> bool {
  t.symbol().map(|sym| sym == s).unwrap_or(false)
}

fn get<'l, V>(m : &'l HashMap<&str, V>, k : Option<&str>) -> Option<&'l V> {
  k.and_then(|k| m.get(k))
}

fn contains_key<'l, V>(m : &'l HashMap<&str, V>, k : Option<&str>) -> bool {
  k.map(|k| m.contains_key(k)).unwrap_or(false)
}

fn contains<'l>(m : &'l HashSet<&str>, k : Option<&str>) -> bool {
  k.map(|k| m.contains(k)).unwrap_or(false)
}

fn expr(tag : ExprTag, content : ExprContent, loc : SrcLocation) -> Expr {
  let ed = ExprData { tag, content, loc };
  perm(ed)
}

impl <'l> ParseState<'l> {

  fn new(
    code_module : Ptr<CodeModule>,
    tokens : Vec<Token<'l>>,
    config : &'l ParseConfig<'static>,
    st : SymbolTable
  )
    -> ParseState<'l>
  {
    ParseState { code_module, tokens, pos: 0, config, st }
  }

  fn has_tokens(&self) -> bool {
    self.pos < self.tokens.len()
  }

  fn prev(&self) -> Option<&Token> {
    if self.pos > 0 {
      return Some(&self.tokens[self.pos-1]);
    }
    None
  }

  fn peek(&self) -> Result<&Token, Error> {
    if self.has_tokens() {
      Ok(&self.tokens[self.pos])
    }
    else {
      let loc = if self.pos > 0 {
        let m = self.tokens[self.pos-1].loc.end;
        SrcLocation{start: m, end: m, module: self.code_module }
      }
      else {
        SrcLocation{start: 0, end: 0, module: self.code_module }
      };
      error(loc, EXPECTED_TOKEN_ERROR)
    }
  }

  fn peek_marker(&self) -> usize {
    if self.has_tokens() {
      self.tokens[self.pos].loc.start
    }
    else if self.tokens.len() == 0 {
      0
    }
    else {
      self.tokens[self.pos-1].loc.end
    }
  }

  fn loc(&self, start : usize) -> SrcLocation {
    if self.pos > 0 {
      let end = self.tokens[self.pos-1].loc.end;
      SrcLocation { start, end, module: self.code_module }
    }
    else {
      SrcLocation { start: 0, end: 0, module: self.code_module }
    }
  }

  fn peek_ahead(&self, offset : usize) -> Option<&Token> {
    let i = self.pos + offset;
    if i < self.tokens.len() {
      Some(&self.tokens[i])
    }
    else {
      None
    }
  }

  fn pop_type(&mut self, token_type : TokenType) -> Result<&Token, Error> {
    let t = self.peek()?;
    if t.token_type != token_type {
      return error(t.loc, format!("Expected token of type '{:?}', found token type '{:?}'", token_type, t.token_type));
    }
    self.skip();
    Ok(&self.tokens[self.pos-1])
  }

  fn pop_syntax(&mut self, str : &str) -> Result<&Token, Error> {
    let t = self.pop_type(TokenType::Symbol)?;
    if t.string == str {
      Ok(t)
    }
    else {
      return error(t.loc,
        format!("Expected syntax '{}', found '{}'", str, t.string));
    }
  }

  fn skip(&mut self) {
    self.pos += 1;
  }

  fn accept(&mut self, string : &str) -> bool {
    let accept = {
      if let Ok(t) = self.peek() {
        match_symbol(t, string)
      }
      else { false }
    };
    if accept { self.skip() }
    accept
  }

  fn expect(&mut self, string : &str) -> Result<(), Error> {
    let t = self.peek()?;
    if !match_symbol(t, string) {
      return error(t.loc, format!("Expected symbol '{}', found {}", string, t));
    }
    self.skip();
    Ok(())
  }

  fn expr(&mut self, tag : ExprTag, content : ExprContent, start : usize) -> Expr {
    let loc = self.loc(start);
    expr(tag, content, loc)
  }

  fn literal_expr(&mut self, val : Val, start : usize) -> Expr {
    let loc = self.loc(start);
    expr(LiteralExpr, ExprContent::LiteralVal(val), loc)
  }

  fn list_expr(&mut self, tag : ExprTag, list : Vec<Expr>, start : usize) -> Expr
  {
    let loc = self.loc(start);
    let content = ExprContent::List(perm_slice_from_vec(list));
    expr(tag, content, loc)
  }

  fn symbol_expr(&mut self, tag : ExprTag, s : Symbol, start : usize) -> Expr {
    let loc = self.loc(start);
    let content = ExprContent::LiteralVal(Val::Symbol(s));
    expr(tag, content, loc)
  }
}

/// This expression parser is vaguely based on some blogs about pratt parsing.
fn pratt_parse(ps : &mut ParseState, precedence : i32) -> Result<Expr, Error> {
  let mut expr = parse_prefix(ps)?;
  while ps.has_tokens() {
    let t = ps.peek()?;
    if contains(&ps.config.paren_terminators, t.symbol()) {
      break;
    }
    // Separators
    else if let Some(&next_precedence) = get(&ps.config.expression_separators, t.symbol()) {
      if next_precedence > precedence {
        let separator = t.symbol().unwrap().as_ref();
        let (tag, separator) = match separator {
          ";" => (Do, ";"),
          "," => (Syntax, ","),
          _ => panic!(),
        };
        ps.pop_type(TokenType::Symbol)?;
        expr = parse_list(ps, vec![expr], separator, tag)?;
      }
      else {
        break;
      }
    }
    // Infix operators
    else if let Some(&next_precedence) = get(&ps.config.infix_precedence, t.symbol()) {
      if next_precedence > precedence {
        // Parens
        if contains_key(&ps.config.paren_pairs, t.symbol()) {
          expr = parse_paren_infix(ps, expr, None)?;
        }
        // Normal infix
        else {
          expr = parse_infix(ps, expr, next_precedence)?;
        }
      }
      else {
        break;
      }
    }
    else {
      break;
    }
  }
  Ok(expr)
}

fn parse_paren_infix(ps : &mut ParseState, left_expr : Expr, first_arg : Option<Expr>) -> Result<Expr, Error> {
  let start = left_expr.loc.start;
  let t = ps.peek()?;
  let (operation, start_paren) = match t.symbol().map(|s| s.as_ref()) {
    Some("(") => (Call, "("),
    Some("[") => (ArrayIndex, "["),
    _ => return error(t.loc, "unexpected token"),
  };
  let end_paren = ps.config.paren_pairs.get(start_paren).unwrap().clone();
  ps.expect(start_paren)?;
  let mut list = vec![left_expr];
  if let Some(first_arg) = first_arg {
    list.push(first_arg);
  }
  parse_into_list(ps, &mut list, ",")?;
  ps.expect(&end_paren)?;
  Ok(ps.list_expr(operation, list, start))
}

fn parse_prefix(ps : &mut ParseState) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let t = ps.peek()?;
  // if the next token is a prefix operator
  if let Some(new_precedence) = get(&ps.config.prefix_precedence, t.symbol()) {
    let t = ps.peek()?;
    match t.string {
      "$" => {
        ps.pop_type(TokenType::Symbol)?;
        let expr = pratt_parse(ps, *new_precedence)?;
        Ok(ps.list_expr(TemplateHole, vec![expr], start)) 
      }
      "#" => {
        ps.pop_type(TokenType::Symbol)?;
        let quoted = pratt_parse(ps, *new_precedence)?;
        Ok(to_template_expr(ps.st, quoted))
      }
      _ => {
        let operator = parse_operator(ps)?;
        let expr = pratt_parse(ps, *new_precedence)?;
        Ok(ps.list_expr(InstrinicOp, vec![operator, expr], start))
      }
    }
  }
  // else assume it's an expression term
  else {
    parse_expression_term(ps)
  }
}

fn parse_infix(ps : &mut ParseState, left_expr : Expr, precedence : i32) -> Result<Expr, Error> {
  let infix_start = left_expr.loc.start;
  let t = ps.peek()?;
  match t.string {
    "." => {
      ps.pop_type(TokenType::Symbol)?;
      let right_expr = pratt_parse(ps, precedence)?;
      Ok(ps.list_expr(FieldIndex, vec![left_expr, right_expr], infix_start))      
    }
    ":" => {
      ps.pop_type(TokenType::Symbol)?;
      let right_expr = pratt_parse(ps, precedence)?;
      Ok(ps.list_expr(Syntax, vec![left_expr, right_expr], infix_start))
    }
    "as" => {
      ps.pop_type(TokenType::Symbol)?;
      let list = vec![left_expr, pratt_parse(ps, precedence)?];
      Ok(ps.list_expr(Cast, list, infix_start))
    }
    _ => {
      let operator = parse_operator(ps)?;
      let list = vec![operator, left_expr, pratt_parse(ps, precedence)?];
      Ok(ps.list_expr(InstrinicOp, list, infix_start))
    }
  }
}

fn parse_into_list(ps : &mut ParseState, list : &mut Vec<Expr>, separator : &str) -> Result<(), Error> {
  let &precedence = ps.config.expression_separators.get(separator).unwrap();
  while ps.has_tokens() {
    let t = ps.peek()?;
    if contains(&ps.config.paren_terminators, t.symbol()) {
      break;
    }
    list.push(pratt_parse(ps, precedence)?);
    if !ps.has_tokens() {
      break;
    }
    if !ps.accept(separator) {
      break;
    }
  }
  Ok(())
}

fn peek_statement_terminated(ps : &ParseState) -> bool {
  if let Ok(t) = ps.peek() {
    let symbol = t.symbol();
    contains(&ps.config.paren_terminators, symbol) ||
      contains_key(&ps.config.expression_separators, symbol)
  }
  else {
    true
  }
}

fn parse_list(ps : &mut ParseState, mut list : Vec<Expr>, separator : &str, tag : ExprTag) -> Result<Expr, Error> {
  let start = list.first().map(|e| e.loc.start).unwrap_or_else(|| ps.peek_marker());
  parse_into_list(ps, &mut list, separator)?;
  Ok(ps.list_expr(tag, list, start))
}

fn parse_block_in_braces(ps : &mut ParseState) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  ps.expect("{")?;
  let mut list = vec![];
  parse_into_list(ps, &mut list, ";")?;
  ps.expect("}")?;
  Ok(ps.list_expr(Do, list, start))
}

fn parse_new_scope(ps : &mut ParseState, precedence : i32) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let e = pratt_parse(ps, precedence)?;
  if e.tag == Do {
    Ok(e)
  }
  else {
    Ok(ps.list_expr(Do, vec![e], start))
  }
}

fn parse_everything(ps : &mut ParseState) -> Result<Expr, Error> {
  pratt_parse(ps, 0)
}

fn parse_literal<T : FromStr>(ps : &mut ParseState) -> Result<T, Error> {
  let t = ps.peek()?;
  let s = t.literal().unwrap().as_ref();
  if let Ok(v) = T::from_str(s) {
    ps.skip();
    Ok(v)
  }
  else {
    error(t.loc, format!("Failed to parse literal from '{}'", s))
  }
}

fn parse_symbol_literal(ps : &mut ParseState) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let st = ps.st;
  let s = ps.pop_type(Symbol)?.string;
  let sym = to_symbol(st, s);
  Ok(ps.symbol_expr(LiteralExpr, sym, start))
}

fn parse_reference(ps : &mut ParseState) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let st = ps.st;
  let s = ps.pop_type(Symbol)?.string;
  let sym = to_symbol(st, s);
  Ok(ps.symbol_expr(Reference, sym, start))
}

fn parse_operator(ps : &mut ParseState) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let t = ps.pop_type(Symbol)?;
  let op = str_to_operator(t.string).ok_or_else(||
    error_raw(t.loc,
      format!("Operator '{}' not supported", t.string)))?;
  Ok(ps.literal_expr(Val::Operator(op), start))
}

fn keyword_precedence(ps : &ParseState) -> i32 {
  *ps.config.prefix_precedence.get("#keyword").unwrap()
}

fn try_parse_keyword_term(ps : &mut ParseState) -> Result<Option<Expr>, Error> {
  let t = ps.peek()?;
  let kp = keyword_precedence(ps);
  let start = ps.peek_marker();
  let symbol = match t.symbol() {
    Some(s) => s.as_ref(), None => return Ok(None),
  };
  let expr = match symbol {
    "if" => {
      ps.pop_type(TokenType::Symbol)?;
      let cond = pratt_parse(ps, kp)?;
      ps.accept("then");
      let then_e = parse_new_scope(ps, kp)?;
      if ps.accept("else") {
        let else_e = parse_new_scope(ps, kp)?;
        ps.list_expr(IfElse, vec![cond, then_e, else_e], start)
      }
      else {
        ps.list_expr(IfElse, vec![cond, then_e], start)
      }
    }
    "while" => {
      ps.pop_type(TokenType::Symbol)?;
      let cond = pratt_parse(ps, kp)?;
      let body = parse_block_in_braces(ps)?;
      let nb = NodeBuilder { loc: ps.loc(start), st: ps.st };
      expr_macros::while_macro(&nb, cond, body)
    }
    "for" => {
      ps.pop_type(TokenType::Symbol)?;
      let loop_var = parse_symbol_literal(ps)?;
      ps.pop_syntax("in")?;
      let start_val = pratt_parse(ps, kp)?;
      ps.pop_syntax("to")?;
      let end_val = pratt_parse(ps, kp)?;
      let body = parse_block_in_braces(ps)?;
      let nb = NodeBuilder { loc: ps.loc(start), st: ps.st };
      expr_macros::for_macro(&nb, loop_var, start_val, end_val, body)
    }
    "fun" => {
      ps.pop_type(TokenType::Symbol)?;
      let mut es = vec![];
      // arguments
      ps.expect("(")?;
      es.push(parse_list(ps, vec![], ",", Syntax)?);
      ps.expect(")")?;
      // return type
      if ps.accept("=>") {
        es.push(pratt_parse(ps, kp)?);
      }
      // body
      es.push(parse_block_in_braces(ps)?);
      ps.list_expr(Fun, es, start)
    }
    "def" => {
      ps.pop_type(TokenType::Symbol)?;
      let name = parse_symbol_literal(ps)?;
      ps.pop_syntax("=")?;
      let value = pratt_parse(ps, kp)?;
      ps.list_expr(Def, vec![name, value], start)
    }
    "let" => {
      ps.pop_type(TokenType::Symbol)?;
      let name = parse_symbol_literal(ps)?;
      ps.pop_syntax("=")?;
      let value = pratt_parse(ps, kp)?;
      ps.list_expr(Def, vec![name, value], start)
    }
    "return" => {
      let start = ps.peek_marker();
      ps.expect("return")?;
      if peek_statement_terminated(ps) {
        ps.list_expr(Return, vec![], start)
      }
      else {
        let return_expr = pratt_parse(ps, kp)?;
        ps.list_expr(Return, vec![return_expr], start)
      }
    }
    "struct" => {
      ps.pop_type(TokenType::Symbol)?;
      ps.expect("{")?;
      let mut fields = vec![];
      parse_into_list(ps, &mut fields, ",")?;
      ps.expect("}")?;
      ps.list_expr(StructType, fields, start)
    }
    "ptr" => {
      ps.pop_type(TokenType::Symbol)?;
      let inner_type = pratt_parse(ps, kp)?;
      ps.list_expr(PtrType, vec![inner_type], start)
    }
    "fn" => {
      parse_function_type(ps, false)?
    }
    // c function type
    "cfun" => {
      parse_function_type(ps, true)?
    }
    // array type
    "sized_array" => {
      ps.pop_type(TokenType::Symbol)?;
      let mut es = vec![];
      ps.expect("(")?;
      es.push(parse_list(ps, vec![], ",", Syntax)?);
      ps.expect(")")?;
      ps.list_expr(SizedArrayType, es, start)
    }
    // slice type
    "slice" => {
      ps.pop_type(TokenType::Symbol)?;
      ps.expect("(")?;
      let element_type = pratt_parse(ps, kp)?;
      ps.expect(")")?;
      let loc = ps.loc(start);
      let nb = NodeBuilder { loc, st: ps.st };
      expr_macros::slice_type_macro(&nb, element_type)
    }
    _ => return Ok(None),
  };
  Ok(Some(expr))
}

fn parse_function_type(ps : &mut ParseState, is_cfun : bool) -> Result<Expr, Error> {
  let start = ps.peek_marker();
  let kp = keyword_precedence(ps);
  ps.pop_type(TokenType::Symbol)?;
  let mut es = vec![];
  // arguments
  ps.expect("(")?;
  es.push(parse_list(ps, vec![], ",", Syntax)?);
  ps.expect(")")?;
  // return type
  ps.expect("=>")?;
  es.push(pratt_parse(ps, kp)?);
  let tag = if is_cfun { CFunType} else { FnType };
  Ok(ps.list_expr(tag, es, start))
}

fn parse_expression_term(ps : &mut ParseState) -> Result<Expr, Error> {
  let t = ps.peek()?;
  match ps.peek()?.token_type {
    Symbol => {
      if let Some(close_paren) = get(&ps.config.paren_pairs, t.symbol()) {
        // Parens
        let start = ps.peek_marker();
        let paren : String = t.to_string();
        ps.pop_type(Symbol)?;
        match paren.as_str() {
          "[" => {
            let e = parse_list(ps, vec![], ",", ArrayInit)?;
            ps.expect(close_paren)?;
            Ok(e)
          }
          "{" => {
            let e = parse_list(ps, vec![], ";", Do)?;
            ps.expect(close_paren)?;
            Ok(e)
          }
          "(" => {
            if ps.accept(close_paren) {
              Ok(ps.literal_expr(Val::Void, start))
            }
            else {
              let e = parse_everything(ps)?;
              ps.expect(close_paren)?;
              Ok(e)
            }
          }
          _ => panic!(),
        }
      }
      else {
        // bools
        let bool_val = t.symbol().and_then(|s| match s.as_ref() {
          "true" => Some(true),
          "false" => Some(false),
          _ => None,
        });
        if let Some(b) = bool_val {
          let start = ps.peek_marker();
          ps.pop_type(TokenType::Symbol)?;
          Ok(ps.literal_expr(Val::Bool(b), start))
        }
          // keyword terms (if, while, etc)
        else if let Some(e) = try_parse_keyword_term(ps)? {
          Ok(e)
        }
        else {
          parse_reference(ps)
        }
      }
    }
    StringLiteral => {
      let start = ps.peek_marker();
      let s = {
        let t = ps.pop_type(StringLiteral)?;
        let s = ffi_libs::from_string(t.to_string());
        perm(s)
      };
      Ok(ps.literal_expr(Val::String(s), start))
    }
    FloatLiteral => {
      let start = ps.peek_marker();
      let f = parse_literal(ps)?;
      Ok(ps.literal_expr(Val::F64(f), start))
    }
    IntLiteral => {
      let start = ps.peek_marker();
      let i = parse_literal(ps)?;
      Ok(ps.literal_expr(Val::I64(i), start))
    }
    Whitespace | Comment | Newline =>
      panic!("whitespace, comment and newline tokens should be filtered out already")
  }
}

fn parse_top_level(ps : &mut ParseState) -> Result<Expr, Error> {
  parse_list(ps, vec![], ";", Do)
}

pub fn parse(module : Ptr<CodeModule>, tokens : Vec<Token>, st : SymbolTable) -> Result<Expr, Error> {
  let config = parse_config();
  let mut ps = ParseState::new(module, tokens, &config, st);
  let e = parse_top_level(&mut ps)?;
  if ps.has_tokens() {
    let t = ps.peek()?;
    return error(t.loc, format!("Unexpected token '{}' of type '{:?}'", t.to_string(), t.token_type));
  }
  return Ok(e);
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
    let nb = NodeBuilder { loc: quoted.loc, st };
    template_macro(&nb, quoted, template_args)
  }
  else {
    expr(LiteralExpr, ExprContent::LiteralVal(Val::Expr(quoted)), quoted.loc)
  }
}

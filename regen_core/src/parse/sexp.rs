/// Parse basic s-expressions into a tree of nodes

use std::str::CharIndices;

use crate::{symbols, perm_alloc, ffi_libs };
use symbols::{Symbol, SymbolTable, to_symbol};
use perm_alloc::{perm, perm_slice, Ptr, SlicePtr};
use ffi_libs::RegenString;

use super::expr::{SrcLocation, CodeModule};

struct TokenStream<'l>{
  code : &'l str,
  st : SymbolTable,
  next_start : usize,
  next_len : usize,
  module : Ptr<CodeModule>,
}

#[derive(Copy, Clone, PartialEq)]
enum TokenType { Normal, Comment, Whitespace, Empty }
use TokenType::*;

#[derive(Copy, Clone)]
struct Token<'l>(TokenType, &'l str);

pub type Node = Ptr<NodeInfo>;

#[derive(Clone, Copy)]
pub enum NodeContent {
  List(SlicePtr<Node>),
  Sym(Symbol),
  Literal(NodeLiteral),
}
use NodeContent::*;

#[derive(Clone, Copy)]
pub enum NodeLiteral {
  I64(i64),
  String(Ptr<RegenString>),
}

fn trim_token(s : &str) -> Token {
  fn is_symbol_char(c : char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
  }
  fn is_bracket(c : char) -> bool {
    match c {
      '(' | ')' | '[' | ']' | '{' | '}' => true,
      _ => false,
    }
  }
  fn is_operator(c : char) -> bool {
    match c {
      '!' | '"' | '#' | '$' | '%' | '&' | '\'' |
      '*' | '+' | ',' | '-' | '.' | '/' | ':' |
      ';' | '<' | '=' | '>' | '?' | '@' | '\\' |
      ']' | '^' | '`' | '|' | '~' => true,
      _ => false,
    }
  }
  fn trim_comment<'l>(s : &'l str, mut it : CharIndices) -> &'l str {
    while let Some((i, c)) = it.next() {
      if c == '\n' { return &s[..i]; }
    }
    return s;
  }
  fn trim_whitespace<'l>(s : &'l str, mut it : CharIndices) -> &'l str {
    while let Some((i, c)) = it.next() {
      if !c.is_ascii_whitespace() { return &s[..i]; }
    }
    return s;
  }
  fn trim_operator<'l>(s : &'l str, mut it : CharIndices) -> &'l str {
    while let Some((i, c)) = it.next() {
      if !is_operator(c) { return &s[..i]; }
    }
    return s;
  }
  fn trim_symbol<'l>(s : &'l str, mut it : CharIndices) -> &'l str {
    while let Some((i, c)) = it.next() {
      if !is_symbol_char(c) { return &s[..i]; }
    }
    return s;
  }
  let it = s.char_indices();
  if s.starts_with(";;") {
    return Token(Comment, trim_comment(s, it));
  }
  if let Some(c) = s.chars().next() {
    if is_bracket(c) {
      return Token(Normal, &s[..1]);
    }
    else if c.is_ascii_whitespace() {
      return Token(Whitespace, trim_whitespace(s, it));
    }
    else if is_operator(c) {
      return Token(Normal, trim_operator(s, it));
    }
    else if is_symbol_char(c) {
      return Token(Normal, trim_symbol(s, it));
    }
    else {
      panic!("unrecognised char")
    }
  };
  return Token(Empty, "");
}

fn peek<'l>(ts : &mut TokenStream<'l>) -> Token<'l> {
  let t = trim_token(&ts.code[ts.next_start..]);
  ts.next_len = t.1.len();
  t
}

fn skip<'l>(ts : &mut TokenStream<'l>) {
  if ts.next_len == 0 {
    peek(ts);
  }
  ts.next_start += ts.next_len;
  ts.next_len = 0;
}

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

#[derive(Clone, Copy)]
pub struct NodeInfo {
  pub loc : SrcLocation,
  pub content : NodeContent,
}

impl NodeInfo {
  pub fn children(&self) -> &[Node] {
    self.perm_children().as_slice()
  }

  pub fn perm_children(&self) -> SlicePtr<Node> {
    match self.content {
      List(l) => l,
      _ => perm_slice(&[]),
    }
  }

  pub fn is_list(&self) -> bool {
    match self.content {
      List(_) => true,
      _ => false,
    }
  }

  pub fn as_symbol(&self) -> Symbol {
    match self.content {
      Sym(s) => s,
      _ => panic!("expected symbol, found '{}'", self.loc.src_snippet()),
    }
  }

  pub fn as_literal_i64(&self) -> i64 {
    match self.content {
      Literal(NodeLiteral::I64(v)) => v,
      _ => panic!("expected literal i64"),
    }
  }
}

fn parse_atom(st : SymbolTable, s : &str) -> NodeContent {
  if let Ok(v) = s.parse::<i64>() {
    Literal(NodeLiteral::I64(v))
  }
  else {
    Sym(to_symbol(st, s))
  }
}

fn parse_string(ts : &mut TokenStream) -> NodeContent {
  let start = ts.next_start;
  let mut end = ts.next_start;
  loop {
    let t = peek(ts);
    if t.0 == Empty { break }
    skip(ts);
    if t.0 == Normal && t.1 == "\"" { break }
    end = ts.next_start;
  }
  let s = ts.code[start..end].to_string();
  let str_ptr = perm(ffi_libs::from_string(s));
  NodeContent::Literal(NodeLiteral::String(str_ptr))
}

/// parse sexp list
fn parse_list(ns : &mut Vec<Node>, ts : &mut TokenStream) {
  let node_index = ns.len();
  let start = ts.next_start;
  loop {
    let t = peek(ts);
    match t.0 {
      Empty => break,
      Comment | Whitespace => {
        skip(ts);
      }
      Normal => {
        match t.1 {
          "" | ")" => {
            break;
          }
          "(" => {
            skip(ts);
            parse_list(ns, ts);
            // TODO handle error properly
            if peek(ts).1 != ")" {
              panic!("syntax error: expected `)`, found `{}`", t.1);
            }
            skip(ts);
          }
          "\"" => {
            let start = ts.next_start;
            skip(ts);
            let content = parse_string(ts);
            let loc = SrcLocation { start, end: ts.next_start, module: ts.module };
            ns.push(perm(NodeInfo{ loc, content }));
          }
          _ => {
            let loc = SrcLocation {
              start: ts.next_start,
              end: ts.next_start + ts.next_len,
              module: ts.module,
            };
            let content = parse_atom(ts.st, &ts.code[loc.start..loc.end]);
            let atom = NodeInfo { loc, content };
            skip(ts);
            ns.push(perm(atom));
          }
        }
      }
    }
  }
  let children = perm_slice(&ns[node_index..]);
  ns.truncate(node_index);
  let loc = SrcLocation { start, end: ts.next_start, module: ts.module };
  ns.push(perm(NodeInfo{ loc, content: NodeContent::List(children) }));
}

pub enum NodeShape<'l> {
  Command(&'l str, &'l [Node]),
  Atom(&'l str),
  Literal(NodeLiteral),
  Other,
}

pub fn node_shape<'l>(n : &'l Node) -> NodeShape<'l> {
  match n.content {
    List(children) => {
      if children.len() > 0 {
        let head = children[0];
        if let Sym(s) = head.content {    
          return NodeShape::Command(s.as_str(), &children.as_slice()[1..]);
        }
      }
      return NodeShape::Other;
    }
    Sym(s) => NodeShape::Atom(s.as_str()),
    Literal(v) => NodeShape::Literal(v),
  }
}

pub fn code_segment(code : &str, n : Node) -> &str {
  &code[n.loc.start..n.loc.end]
}

pub fn sexp_list(st : SymbolTable, module_name : &str, code : &str) -> Node {
  let mut ns = vec!();
  let module = perm(CodeModule {
    name: module_name.to_string(),
    code: code.to_string(),
  });
  let mut ts = TokenStream { code, st, next_start: 0, next_len: 0, module };
  parse_list(&mut ns, &mut ts);
  loop {
    let t = peek(&mut ts);
    match t.0 {
      Empty => break,
      Comment | Whitespace => {
        skip(&mut ts);
      }
      Normal => {
        panic!("unexpected token `{}`", t.1);
      }
    }
  }
  // display(code, 0, ns[0], &mut false);
  // println!();
  ns.pop().unwrap()
}

pub fn sexp(st : SymbolTable, module_name : &str, code : &str) -> Node {
  let n = sexp_list(st, module_name, code);
  if let &[n] = n.children() {
    return n;
  }
  panic!("expected single expression")
}

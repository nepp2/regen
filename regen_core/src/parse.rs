/// Parse basic s-expressions into a tree of nodes

use std::str::CharIndices;

use crate::{symbols, perm_alloc, interop };
use symbols::{Symbol, SymbolTable, to_symbol};
use perm_alloc::{perm, perm_slice, Perm, PermSlice};
use interop::RegenString;

struct TokenStream<'l>{
  code : &'l str,
  st : SymbolTable,
  next_start : usize,
  next_len : usize,
  perm_code : Perm<String>,
}

#[derive(Copy, Clone, PartialEq)]
enum TokenType { Normal, Comment, Whitespace, Empty }
use TokenType::*;

#[derive(Copy, Clone)]
struct Token<'l>(TokenType, &'l str);

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

pub type Node = Perm<NodeInfo>;

#[derive(Clone, Copy)]
pub enum NodeContent {
  List(PermSlice<Node>),
  Sym(Symbol),
  Literal(NodeLiteral),
}

#[derive(Clone, Copy)]
pub enum NodeLiteral {
  U64(u64),
  String(Perm<RegenString>),
}

use NodeContent::*;

#[derive(Clone, Copy, Debug)]
pub struct SrcLocation {
  pub start : usize,
  pub end : usize,  
  pub code : Perm<String>,
}

impl SrcLocation {
  pub fn zero() -> Self {
    SrcLocation {
      start: 0, end: 0, code: Perm { p : std::ptr::null_mut() },
    }
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

  pub fn perm_children(&self) -> PermSlice<Node> {
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
      _ => panic!("expected symbol, found {}", self),
    }
  }

  pub fn as_literal_u64(&self) -> u64 {
    match self.content {
      Literal(NodeLiteral::U64(v)) => v,
      _ => panic!("expected literal u64"),
    }
  }
}

fn parse_atom(st : SymbolTable, s : &str) -> NodeContent {
  if let Ok(v) = s.parse::<u64>() {
    Literal(NodeLiteral::U64(v))
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
  let s = interop::from_string(ts.perm_code[start..end].to_string());
  NodeContent::Literal(NodeLiteral::String(perm(s)))
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
              panic!("syntax error: unbalanced list");
            }
            skip(ts);
          }
          "\"" => {
            let start = ts.next_start;
            skip(ts);
            let content = parse_string(ts);
            let loc = SrcLocation { start, end: ts.next_start, code: ts.perm_code };
            ns.push(perm(NodeInfo{ loc, content }));
          }
          _ => {
            let loc = SrcLocation {
              start: ts.next_start,
              end: ts.next_start + ts.next_len,
              code: ts.perm_code,
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
  let loc = SrcLocation { start, end: ts.next_start, code: ts.perm_code };
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

pub fn parse(st : SymbolTable, code : &str) -> Node {
  let mut ns = vec!();
  let perm_code = perm(code.to_string());
  let mut ts = TokenStream { code, st, next_start: 0, next_len: 0, perm_code };
  parse_list(&mut ns, &mut ts);
  // display(code, 0, ns[0], &mut false);
  // println!();
  ns.pop().unwrap()
}

use std::fmt;

impl fmt::Display for SrcLocation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut line_start = 0;
    let mut line_number = 1;
    for (i, c) in self.code.as_str()[0..self.start].char_indices() {
      if c == '\n' {
        line_start = i;
        line_number += 1;
      }
    }
    write!(f, "line {}, {} to {}",
      line_number,
      self.start - line_start,
      self.end - line_start)
  }
}

impl fmt::Display for NodeInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    display_node(f, 0, self, &mut false)
  }
}

fn display_node(f: &mut fmt::Formatter<'_>, depth: usize, n : &NodeInfo, newline : &mut bool) -> fmt::Result {
  match n.content {
    List(children) => display_list(f, depth, children.as_slice(), newline),
    Sym(s) => write!(f, "{}", s),
    Literal(l) => display_literal(f, &l),
  }
}

fn display_literal(f: &mut fmt::Formatter<'_>, l : &NodeLiteral) -> fmt::Result {
  match l {
    NodeLiteral::U64(v) => write!(f, "{}", v),
    NodeLiteral::String(s) => write!(f, "\"{}\"", s.as_str()),
  }
}

fn display_list(f: &mut fmt::Formatter<'_>, depth: usize, ns : &[Node], newline : &mut bool) -> fmt::Result {
  write!(f, "(")?;
  for i in 0..ns.len() {
    let n = ns[i];
    if n.children().len() > 0 && (ns.len() > 3 || i == 0) {
      display_list_newline(f, depth + 1, ns, i, true)?;
      *newline = true;
      return Ok(());
    }
    if i > 0 {
      write!(f, " ")?;
    }
    display_node(f, depth, &*n, newline)?;
    if *newline {
      return display_list_newline(f, depth + 1, ns, i + 1, false);
    }
  }
  write!(f, ")")
}

fn display_list_newline(f: &mut fmt::Formatter<'_>, depth: usize, ns : &[Node], mut i : usize, indent_newline : bool) -> fmt::Result {
  while i < ns.len() {
    writeln!(f)?;
    print_indent(f, depth * 2)?;
    display_node(f, depth, &*ns[i], &mut false)?;
    i += 1;
  }
  if indent_newline {
    writeln!(f)?;
    print_indent(f, (depth-1) * 2)?;
  }
  write!(f, ")")
}

fn print_indent(f: &mut fmt::Formatter<'_>, indent : usize) -> fmt::Result {
  for _ in 0..indent { write!(f, " ")? }
  Ok(())
}

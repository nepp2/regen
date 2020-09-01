/// Parse basic s-expressions into a tree of nodes

use std::str::CharIndices;

use crate::symbols::{Symbol, SymbolTable, to_symbol};
use crate::perm_alloc::{perm, perm_slice, Perm, PermSlice};

struct TokenStream<'l>{
  code : &'l str,
  st : SymbolTable,
  next_start : usize,
  next_len : usize,
  perm_code : Perm<String>,
}

#[derive(Copy, Clone)]
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

#[derive(Clone, Copy, Debug)]
pub enum NodeContent {
  List(PermSlice<Node>),
  Sym(Symbol),
  Literal(u64),
}

use NodeContent::*;

#[derive(Clone, Copy, Debug)]
pub struct SrcLocation {
  pub start : usize,
  pub end : usize,  
  pub code : Perm<String>,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeInfo {
  pub loc : SrcLocation,
  pub content : NodeContent,
}

impl NodeInfo {
  pub fn children(&self) -> &[Node] {
    match self.content {
      NodeContent::List(l) => l.as_slice(),
      _ => &[],
    }
  }

  pub fn is_list(&self) -> bool {
    match self.content {
      NodeContent::List(_) => true,
      _ => false,
    }
  }

  pub fn as_symbol(&self) -> Symbol {
    match self.content {
      NodeContent::Sym(s) => s,
      _ => panic!("expected symbol, found {:?}", self),
    }
  }

  pub fn as_literal(&self) -> u64 {
    match self.content {
      NodeContent::Literal(v) => v,
      _ => panic!("expected literal"),
    }
  }
}

fn parse_atom(st : SymbolTable, s : &str) -> NodeContent {
  if let Ok(v) = s.parse::<u64>() {
    Literal(v)
  }
  else {
    Sym(to_symbol(st, s))
  }
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
  Atom(&'l str, Symbol),
  Literal(u64),
  Other,
}

pub fn node_shape<'l>(n : &'l Node) -> NodeShape<'l> {
  match n.content {
    List(children) => {
      let head = children[0];
      if let Sym(s) = head.content {    
        NodeShape::Command(s.as_str(), &children.as_slice()[1..])
      }
      else {
        NodeShape::Other
      }
    }
    Sym(s) => NodeShape::Atom(s.as_str(), s),
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

impl fmt::Display for Node {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    display(f, 0, *self, &mut false)
  }
}

fn display(f: &mut fmt::Formatter<'_>, depth: usize, n : Node, newline : &mut bool) -> fmt::Result {
  match n.content {
    List(children) => display_list(f, depth, children.as_slice(), newline),
    Sym(s) => write!(f, "{}", s),
    Literal(l) => write!(f, "{}", l),
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
    display(f, depth, n, newline)?;
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
    display(f, depth, ns[i], &mut false)?;
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

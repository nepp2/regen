

use crate::symbols;
use symbols::Symbol;

use crate::perm_alloc::{perm, perm_slice, Perm, PermSlice};

struct TokenStream<'l>{
  code : &'l str,
  next_start : usize,
  next_len : usize,
}

fn is_divider(c : char) -> bool {
  match c {
    ' ' | '\n' | '\r' | '\t' | '(' | ')' => true,
    _ => false,
  }
}

fn trim_token(s : &str) -> &str {
  let mut it = s.char_indices();
  if let Some((_, c)) = it.next() {
    if is_divider(c) {
      return &s[..1]
    }
  }
  while let Some((i, c)) = it.next() {
    if is_divider(c) {
      return &s[..i];
    }
  }
  s
}

fn peek<'l>(ts : &mut TokenStream<'l>) -> &'l str {
  let s = trim_token(&ts.code[ts.next_start..]);
  ts.next_len = s.len();
  s
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
pub struct NodeInfo {
  pub start : usize,
  pub end : usize,
  pub children : PermSlice<Node>,
}

/// parse sexp list
fn parse_list(ns : &mut Vec<Node>, ts : &mut TokenStream) {
  let node_index = ns.len();
  let start = ts.next_start;
  loop {
    match peek(ts) {
      "" | ")" => {
        break;
      }
      " " | "\n" | "\r" | "\t" => {
        skip(ts);
      }
      "(" => {
        skip(ts);
        parse_list(ns, ts);
        // TODO handle error properly
        if peek(ts) != ")" {
          panic!("syntax error: unbalanced list");
        }
        skip(ts);
      }
      _ => {
        let atom = NodeInfo {
          start: ts.next_start,
          end: ts.next_start + ts.next_len,
          children: perm_slice(&[])
        };
        skip(ts);
        ns.push(perm(atom));
      }
    }
  }
  let children = perm_slice(&ns[node_index..]);
  ns.truncate(node_index);
  ns.push(perm(NodeInfo{ start, end: ts.next_start, children }));
}

pub fn to_symbol(code : &str, n : Node) -> Symbol {
  symbols::to_symbol(code_segment(code, n))
}

pub enum NodeShape<'l> {
  Command(&'l str, &'l [Node]),
  Atom(&'l str),
  Other,
}

pub fn node_shape<'l>(n : &'l Node, code : &'l str) -> NodeShape<'l> {
  if n.children.len() > 0 {
    let head = n.children[0];
    if head.children.len() == 0 {    
      let s = code_segment(code, head);
      NodeShape::Command(s, &n.children[1..])
    }
    else {
      NodeShape::Other
    }
  }
  else {
    NodeShape::Atom(code_segment(code, *n))
  }
}

pub fn head_tail<'l>(n : &'l Node, code : &'l str) -> Option<(&'l str, &'l [Node])> {
  if n.children.len() > 0 {
    let s = code_segment(code, n.children[0]);
    return Some((s, &n.children[1..]));
  }
  None
}

pub fn code_segment(code : &str, n : Node) -> &str {
  &code[n.start..n.end]
}

pub fn parse(code : &str) -> Node {
  let mut ns = vec!();
  let mut ts = TokenStream { code, next_start: 0, next_len: 0 };
  parse_list(&mut ns, &mut ts);
  display(code, 0, ns[0]);
  ns.pop().unwrap()
}

fn display(code : &str, indent: usize, n : Node) {
  if n.children.len() == 0 {
    print_indent(indent);
    println!("{}", &code[n.start..n.end]);
  }
  else {
    print_indent(indent);
    print!("(");
    let mut i = 0;
    let cs = n.children;
    'outer: loop {
      while i < cs.len() {
        let c = cs[i];
        if c.end - c.start > 20 {
          break 'outer;
        }
        if i > 0 {
          print!(" ");
        }
        display_inline(code, c);
        i += 1;
      }
      print!(")");
      return;
    }
    while i < cs.len() {
      let c = cs[i];
      println!();
      display(code, indent + 2, c);
      i += 1;
    }
    print_indent(indent);
    print!(")");
  }
}

fn print_indent(indent : usize) {
  for _ in 0..indent { print!(" ") }
}

fn display_inline(code : &str, n : Node) {
  if n.children.len() == 0 {
    print!("{}", &code[n.start..n.end]);
  }
  else {
    print!("(");
    display_inline(code, n.children[0]);
    for &c in  &n.children[1..] {
      print!(" ");
      display_inline(code, c);
    }
    print!(")");
  }
}
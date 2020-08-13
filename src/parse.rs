

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
  display(code, 0, ns[0], &mut false);
  println!();
  ns.pop().unwrap()
}

fn display(code : &str, depth: usize, n : Node, newline : &mut bool) {
  if n.children.len() == 0 {
    print!("{}", &code[n.start..n.end]);
  }
  else {
    display_list(code, depth, n.children.as_slice(), newline);
  }
}

fn display_list(code : &str, depth: usize, ns : &[Node], newline : &mut bool) {
  print!("(");
  for i in 0..ns.len() {
    let n = ns[i];
    if n.children.len() > 0 && (ns.len() > 3 || i == 0) {
      display_list_newline(code, depth + 1, ns, i, true);
      *newline = true;
      return;
    }
    if i > 0 {
      print!(" ");
    }
    display(code, depth, n, newline);
    if *newline {
      display_list_newline(code, depth + 1, ns, i + 1, false);
      return;
    }
  }
  print!(")");
}

fn display_list_newline(code : &str, depth: usize, ns : &[Node], mut i : usize, indent_newline : bool) {
  while i < ns.len() {
    println!();
    print_indent(depth * 2);
    display(code, depth, ns[i], &mut false);
    i += 1;
  }
  if indent_newline {
    println!();
    print_indent((depth-1) * 2);
  }
  print!(")");
}

fn print_indent(indent : usize) {
  for _ in 0..indent { print!(" ") }
}

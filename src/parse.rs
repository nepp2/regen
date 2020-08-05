

use crate::symbols;
use symbols::Symbol;

use std::str::CharIndices;
use std::iter::Peekable;

use crate::perm_alloc::{perm, perm_slice, Perm, PermSlice};

type CharStream<'l> = Peekable<CharIndices<'l>>;

pub type Node = Perm<NodeInfo>;

#[derive(Clone, Copy, Debug)]
pub struct NodeInfo {
  pub start : usize,
  pub end : usize,
  pub children : PermSlice<Node>,
}

/// parse sexp list
fn parse_list(ns : &mut Vec<Node>, cs : &mut CharStream, start : usize) {
  let node_index = ns.len();
  let mut pos = start;
  while let Some((i, c)) = cs.peek() {
    pos = *i;
    match c {
      ')' => {
        pos += 1;
        break;
      }
      ' ' | '\n' | '\r' | '\t' => {
        cs.next();
      }
      '(' => {
        cs.next();
        parse_list(ns, cs, pos);
        // TODO handle error properly
        if cs.next().unwrap().1 != ')' {
          panic!();
        }
      }
      _ => {
        ns.push(perm(parse_atom(cs, pos)));
      }
    }
  }
  let children = perm_slice(&ns[node_index..]);
  ns.truncate(node_index);
  ns.push(perm(NodeInfo{ start, end: pos, children }));
}

/// parse sexp atom
fn parse_atom(cs : &mut CharStream, start : usize) -> NodeInfo {
  let mut end = start;
  while let Some((i, c)) = cs.peek() {
    end = *i;
    match c {
      ' ' | '\n' | '\r' | '\t' | '(' | ')' => {
        break
      }
      _ => {
        cs.next();
      }
    }
  }
  NodeInfo {start, end, children: perm_slice(&[]) }
}

pub fn to_symbol(code : &str, n : Node) -> Symbol {
  symbols::to_symbol(code_segment(code, n))
}

// TODO: this is called repeatedly on the same node, which might be a bit slow
pub fn match_head<'l>(n : &'l Node, code : &str, s : &str) -> Option<&'l [Node]> {
  if n.children.len() > 0 {
    if code_segment(code, n.children[0]) == s {
      return Some(&n.children[1..]);
    }
  }
  None
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
  let mut cs = code.char_indices().peekable();
  parse_list(&mut ns, &mut cs, 0);
  ns.pop().unwrap()
}


use std::str::CharIndices;
use std::iter::Peekable;

type CharStream<'l> = Peekable<CharIndices<'l>>;

pub struct AbstractSyntaxTree {
  pub code : String,
  pub nodes : Vec<Node>,
  pub child_indices : Vec<usize>,
}

#[derive(Copy, Clone, Debug)]
pub struct Node {
  pub num_children : u32,
  pub children_offset : u32,
  pub start : usize,
  pub end : usize,
}

fn find_child_indices(
  ns : &mut [Node],
  child_indices : &mut Vec<usize>)
{
  fn find(
    ns : &mut [Node],
    child_indices : &mut Vec<usize>,
    node_index : &mut usize)
  {
    let n = &mut ns[*node_index];
    if n.num_children > 0 {
      let offset = child_indices.len() as u32;
      n.children_offset = offset;
      for _ in 0..n.num_children {
        child_indices.push(0);
      }
      for i in 0..n.num_children {
        *node_index += 1;
        child_indices[(offset + i) as usize] = *node_index;
        find(ns, child_indices, node_index);
      }
    }
  }
  let mut index = 0;
  find(ns, child_indices, &mut index);
}

/// parse sexp list
fn parse_list(ns : &mut Vec<Node>, cs : &mut CharStream, start : usize) {
  let node_index = ns.len();
  let node = Node{ num_children: 0, children_offset: 0, start, end: 0 };
  ns.push(node);
  let mut num_children = 0;
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
        num_children += 1;
      }
      _ => {
        ns.push(parse_atom(cs, pos));
        num_children += 1;
      }
    }
  }
  let n = &mut ns[node_index];
  n.num_children = num_children;
  n.end = pos;
}

/// parse sexp atom
fn parse_atom(cs : &mut CharStream, start : usize) -> Node {
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
  Node {num_children: 0, children_offset: 0, start, end }
}

pub fn node_children<'l>(n : &Node, child_indices : &'l [usize]) -> &'l [usize] {
  let start = n.children_offset as usize;
  let end = start + n.num_children as usize;
  &child_indices[start..end]
}

pub fn parse(s : String) -> AbstractSyntaxTree {
  let mut ns = vec!();
  let mut cs = s.char_indices().peekable();
  parse_list(&mut ns, &mut cs, 0);
  let mut child_indices = vec!(0 ; ns.len()-1);
  find_child_indices(&mut ns, &mut child_indices);
  AbstractSyntaxTree {
    code: s,
    nodes: ns,
    child_indices,
  }
}
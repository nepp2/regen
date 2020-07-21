

use std::str::CharIndices;
use std::iter::Peekable;

type CharStream<'l> = Peekable<CharIndices<'l>>;

#[derive(Copy, Clone)]
pub struct NodeIndex(pub usize);

pub struct AbstractSyntaxTree {
  pub code : String,
  pub node_info : Vec<NodeInfo>,
  pub child_indices : Vec<NodeIndex>,
}

impl AbstractSyntaxTree {
  /// Returns the root node index, which is always at 0
  pub fn root() -> NodeIndex {
    NodeIndex(0)
  }
}

#[derive(Copy, Clone, Debug)]
pub struct NodeInfo {
  pub num_children : u32,
  pub children_offset : u32,
  pub start : usize,
  pub end : usize,
}

fn find_child_indices(
  ns : &mut [NodeInfo],
  child_indices : &mut Vec<NodeIndex>)
{
  fn find(
    ns : &mut [NodeInfo],
    child_indices : &mut Vec<NodeIndex>,
    node_index : &mut NodeIndex)
  {
    let n = &mut ns[node_index.0];
    if n.num_children > 0 {
      let offset = child_indices.len() as u32;
      n.children_offset = offset;
      for _ in 0..n.num_children {
        child_indices.push(NodeIndex(0));
      }
      for i in 0..n.num_children {
        node_index.0 += 1;
        child_indices[(offset + i) as usize] = *node_index;
        find(ns, child_indices, node_index);
      }
    }
  }
  let mut index = NodeIndex(0);
  find(ns, child_indices, &mut index);
}

/// parse sexp list
fn parse_list(ns : &mut Vec<NodeInfo>, cs : &mut CharStream, start : usize) {
  let node_index = ns.len();
  let node = NodeInfo{ num_children: 0, children_offset: 0, start, end: 0 };
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
  NodeInfo {num_children: 0, children_offset: 0, start, end }
}

pub fn code_segment(ast : &AbstractSyntaxTree, n : NodeIndex) -> &str {
  let n = &ast.node_info[n.0];
  &ast.code[n.start..n.end]
}

pub fn node_children<'l>(ast : &'l AbstractSyntaxTree, n : NodeIndex) -> &'l [NodeIndex] {
  let n = &ast.node_info[n.0];
  let start = n.children_offset as usize;
  let end = start + n.num_children as usize;
  &ast.child_indices[start..end]
}

pub fn parse(s : String) -> AbstractSyntaxTree {
  let mut ns = vec!();
  let mut cs = s.char_indices().peekable();
  parse_list(&mut ns, &mut cs, 0);
  let mut child_indices = vec!(NodeIndex(0) ; ns.len()-1);
  find_child_indices(&mut ns, &mut child_indices);
  AbstractSyntaxTree {
    code: s,
    node_info: ns,
    child_indices,
  }
}
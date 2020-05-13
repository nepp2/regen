
use crate::{parse, symbols};
use parse::{AbstractSyntaxTree as AST};
use symbols::Symbol;

#[derive(Copy, Clone, Debug)]
struct BlockId(u64);

#[derive(Copy, Clone, Debug)]
pub struct Op {
  pub varname : Option<Symbol>,
  pub content : OpContent,
}

#[derive(Copy, Clone, Debug)]
pub struct OpResult(usize);

#[derive(Copy, Clone, Debug)]
pub enum OpContent {
  LiteralU8(u8),
  Set{ src_ptr: OpResult, dest_ptr: OpResult, size: u32},
  CJump{ cond: OpResult, then_block: BlockId, else_block: BlockId },
  Jump(BlockId),
  Exit,
  //Add{ a: RegId, b: RegId },
  //Error,
}

pub struct Block {
  pub id : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
}

#[derive(Default)]
pub struct ByteCode {
  pub blocks : Vec<Block>,
  pub ops : Vec<Op>,
  pub frame_byte_size : usize,
}

pub fn codegen(ast : &AST) -> ByteCode {
  let mut bc = Default::default();
  let root_node = 0;
  for &i in node_children(ast, root_node) {
    gen_block(&mut bc, ast, i);
  }
  bc
}

fn node_children(ast : &AST, n : usize) -> &[usize] {
  let n = &ast.nodes[n];
  parse::node_children(n, &ast.child_indices)
}

fn code_segment(ast : &AST, n : usize) -> &str {
  let n = &ast.nodes[n];
  &ast.code[n.start..n.end]
}

fn to_symbol(ast : &AST, n : usize) -> Symbol {
  symbols::to_symbol(code_segment(ast, n))
}

fn match_head<'l>(ast : &'l AST, n : usize, s : &str) -> Option<&'l [usize]> {
  let cs = node_children(ast, n);
  if cs.len() > 0 {
    if code_segment(ast, cs[0]) == s {
      return Some(&cs[1..]);
    }
  }
  None
}

fn gen_block(bc : &mut ByteCode, ast : &AST, n : usize) {
  if let Some(tail) = match_head(ast, n, "block") {
    let mut block = Block {
      id: to_symbol(ast, tail[0]),
      start_op: bc.ops.len(),
      num_ops: 0,
    };
    for &n in &tail[1..] {
      let op = gen_instruction(bc, &mut block, ast, n);
      bc.ops.push(op);
      block.num_ops += 1;
    }
    bc.blocks.push(block);
    return;
  }
  panic!("expected block")
}

fn gen_instruction(bc : &mut ByteCode, block : &mut Block, ast : &AST, n : usize) -> Op {
  // literal bind
  if let Some([varname, value]) = match_head(ast, n, "let") {
    let varname = to_symbol(ast, *varname);
    let value = match code_segment(ast, *value) {
      "true" => 1,
      "false" => 0,
      _ => panic!("unknown value"),
    };
    return Op {
      varname: Some(varname),
      content: OpContent::LiteralU8(value),
    };
  }
  // conditional jump
  if let Some([cond, then_block, else_block]) = match_head(ast, n, "cjump") {
    let cond = {
      let c = to_symbol(ast, *cond);
      let v =
        bc.ops[block.start_op..].iter()
        .position(|op| op.varname == Some(c))
        .expect("expected var");
      OpResult(v + block.start_op)
    };
    let then_block = to_symbol(ast, *then_block);
    let else_block = to_symbol(ast, *else_block);
    return Op {
      varname: None,
      content: OpContent::CJump{cond, then_block, else_block},
    };
  }
  // Jump
  if let Some([block]) = match_head(ast, n, "jump") {
    let block = to_symbol(ast, *block);
    return Op {
      varname: None,
      content: OpContent::Jump(block),
    };
  }
  // Exit
  if code_segment(ast, n) == "exit" {
    return Op {
      varname: None,
      content: OpContent::Exit,
    };
  }
  panic!("unrecognised instruction")
}


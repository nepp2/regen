
use crate::{parse, symbols};
use parse::{
  AbstractSyntaxTree as AST,
  NodeIndex, node_children,
  code_segment,
};
use symbols::Symbol;

#[derive(Copy, Clone, Debug)]
pub struct BlockIndex(pub usize);

/// Register local to block, indexed from 0 upward
#[derive(Copy, Clone, Debug)]
pub struct RegIndex(pub usize);

#[derive(Copy, Clone, Debug)]
pub enum Expr {
  Symbol(Symbol),
  LiteralU64(u64),
  Add(RegIndex, RegIndex),
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
  Expr(RegIndex, Expr),
  Set(RegIndex, RegIndex),
  CJump{ cond: RegIndex, then_block: BlockIndex, else_block: BlockIndex },
  Debug(Symbol),
  Jump(BlockIndex),
  Call(RegIndex),
  Exit,
  //Error,
}

pub struct Block {
  pub name : Symbol,
  pub start_op : usize,
  pub num_ops : usize,
  pub registers : usize,
}

#[derive(Default)]
pub struct ByteCode {
  pub blocks : Vec<Block>,
  pub ops : Vec<Op>,
  pub frame_byte_size : usize,
}

pub struct Function {
  pub bytecode : ByteCode
}

type BlockInfo<'l> = Vec<(Symbol, &'l [NodeIndex])>;

pub fn codegen(ast : &AST) -> ByteCode {
  let children = node_children(ast, AST::root());
  codegen_function(ast, children[0]).bytecode
}

pub fn codegen_function(ast : &AST, root : NodeIndex) -> Function {
  if let Some([args, body]) = match_head(ast, root, "fun") {
    return Function { bytecode: codegen_bytecode(ast, *body)};
  }
  panic!("expected function")
}

fn codegen_bytecode(ast : &AST, root : NodeIndex) -> ByteCode {
  let mut bc : ByteCode = Default::default();
  let children = node_children(ast, root);
  let mut blocks = vec![];
  let mut locals = vec![];
  let mut registers = 0;
  // Find all blocks (they can be out of order)
  for &node in children {
    if let Some(tail) = match_head(ast, node, "vars") {
      for var in tail {
        let name = to_symbol(ast, *var);
        locals.push((name, next_reg(&mut registers)));
      }
    }
    else if let Some(tail) = match_head(ast, node, "block") {
      let name = to_symbol(ast, tail[0]);
      let instructions = &tail[1..];
      blocks.push((name, instructions));
    }
    else {
      panic!("expected block")
    }
  }
  // Generation block instructions
  for (name, instructions) in blocks.as_slice() {
    let start_op = bc.ops.len();
    for &n in *instructions {
      gen_instruction(&mut bc, &blocks, &mut locals, &mut registers, ast, n);
    }
    let num_ops = bc.ops.len() - start_op;
    bc.blocks.push(Block {name: *name, start_op, num_ops, registers });
  }
  bc
}

fn to_symbol(ast : &AST, n : NodeIndex) -> Symbol {
  symbols::to_symbol(code_segment(ast, n))
}

fn to_block_id(blocks : &BlockInfo, s : Symbol) -> BlockIndex {
  let i = blocks.iter().position(|x| x.0 == s).unwrap();
  BlockIndex(i)
}

fn match_head<'l>(ast : &'l AST, n : NodeIndex, s : &str) -> Option<&'l [NodeIndex]> {
  let cs = node_children(ast, n);
  if cs.len() > 0 {
    if code_segment(ast, cs[0]) == s {
      return Some(&cs[1..]);
    }
  }
  None
}

fn next_reg(registers : &mut usize) -> RegIndex {
  let r = RegIndex(*registers);
  *registers += 1;
  r
}

fn try_gen_expr(
  bc : &mut ByteCode,
  blocks : &BlockInfo,
  locals : &mut Vec<(Symbol, RegIndex)>,
  registers : &mut usize,
  ast : &AST,
  node : NodeIndex
) -> Option<Expr> {
  if let Some([a, b]) = match_head(ast, node, "+") {
    let a = gen_value(bc, blocks, locals, registers, ast, *a);
    let b = gen_value(bc, blocks, locals, registers, ast, *b);
    return Some(Expr::Add(a, b));
  }
  let segment = code_segment(ast, node);
  // boolean literals
  match segment {
    "true" => return Some(Expr::LiteralU64(1)),
    "false" => return Some(Expr::LiteralU64(0)),
    _ => (),
  }
  // integer literals
  if let Ok(v) = segment.parse::<u64>() {
    return Some(Expr::LiteralU64(v));
  }
  None
}

fn gen_value(
  bc : &mut ByteCode,
  blocks : &BlockInfo,
  locals : &mut Vec<(Symbol, RegIndex)>,
  registers : &mut usize,
  ast : &AST,
  node : NodeIndex
) -> RegIndex {
  if let Some(e) = try_gen_expr(bc, blocks, locals, registers, ast, node) {
    let reg = next_reg(registers);
    bc.ops.push(Op::Expr(reg, e));
    return reg;    
  }
  // Look for local
  if let Some(v) = find_local(locals, ast, node) {
    return v;
  }
  panic!("invalid expression")
}

fn find_local(locals : &mut Vec<(Symbol, RegIndex)>, ast : &AST, node : NodeIndex)
  -> Option<RegIndex>
{
  let c = to_symbol(ast, node);
  locals.iter()
    .find(|&l| l.0 == c).map(|v| v.1)
}

fn gen_instruction(
  bc : &mut ByteCode,
  blocks : &BlockInfo,
  locals : &mut Vec<(Symbol, RegIndex)>,
  registers : &mut usize,
  ast : &AST,
  node : NodeIndex,
) {
  let op = {
    // set var
    if let Some([varname, value]) = match_head(ast, node, "set") {
      let var_reg = find_local(locals, ast, *varname).expect("no variable found");
      let val_reg = gen_value(bc, blocks, locals, registers, ast, *value);
      Op::Set(var_reg, val_reg)
    }
    // conditional jump
    else if let Some([cond, then_block, else_block]) = match_head(ast, node, "cjump") {
      let cond = gen_value(bc, blocks, locals, registers, ast, *cond);
      let then_block = to_block_id(blocks, to_symbol(ast, *then_block));
      let else_block = to_block_id(blocks, to_symbol(ast, *else_block));
      Op::CJump{cond, then_block, else_block}
    }
    // Jump
    else if let Some([block]) = match_head(ast, node, "jump") {
      let block = to_block_id(blocks, to_symbol(ast, *block));
      Op::Jump(block)
    }
    // Debug
    else if let Some([s]) = match_head(ast, node, "debug") {
      Op::Debug(to_symbol(ast, *s))
    }
    // Exit
    else if code_segment(ast, node) == "exit" {
      Op::Exit
    }
    else {
      panic!("unrecognised instruction")
    }
  };
  bc.ops.push(op);
}

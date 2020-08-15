
use super::definition::*;

use crate::{parse, symbols, env};
use env::Env;
use parse::{
  Node,
  code_segment, to_symbol,
  node_shape, NodeShape::*,
};
use symbols::Symbol;

/// Sequence instructions
struct SeqInstrs<'l> {
  name : Symbol,
  instructions : &'l [Node],
}

/// Read bytecode from a simple basic-block s-expression representation
pub fn read_bytecode(env: &Env, code : &str, root : Node) -> BytecodeFunction {
  let mut locals = vec![];
  let mut registers = 0;
  let body;
  let args;
  if let Command("fun", [arg_nodes, body_node]) = node_shape(&root, code) {
    body = *body_node;
    args = arg_nodes.children.len();
    for &arg in arg_nodes.children {
      let name = to_symbol(env.st, code, arg);
      let reg = next_reg(&mut registers);
      locals.push(LocalVar{ name, reg });
    }
  }
  else {
    panic!("expected function")
  }
  let mut seq_instrs = vec![];
  // Find all sequences (they can be out of order)
  for node in body.children {
    match node_shape(node, code) {
      Command("vars", tail) => {
        for &var in tail {
          let name = to_symbol(env.st, code, var);
          let reg = next_reg(&mut registers);
          locals.push(LocalVar{ name, reg });
        }
      }
      Command("seq", tail) => {
        let name = to_symbol(env.st, code, tail[0]);
        let instructions = &tail[1..];
        seq_instrs.push(SeqInstrs{ name, instructions });
      }
      _ => {
        panic!("expected seq")
      }
    }
  }
  // Generation sequence instructions
  let mut ops = vec![];
  let mut b = Builder {
    ops: &mut ops,
    sequence_info: &seq_instrs,
    locals: &mut locals,
    registers: &mut registers,
    code,
    env,
    arg_values: vec![],
  };
  let mut sequence_info = vec![];
  for bi in seq_instrs.as_slice() {
    let start_op = b.ops.len();
    for &n in bi.instructions {
      read_instruction(&mut b, n);
    }
    let num_ops = b.ops.len() - start_op;
    sequence_info.push(SequenceInfo {name: bi.name, start_op, num_ops });
  }
  BytecodeFunction { sequence_info, ops, registers, args, locals }
}

fn to_sequence_id(sequences : &Vec<SeqInstrs>, s : Symbol) -> SeqenceId {
  let i = sequences.iter().position(|b| b.name == s).unwrap();
  SeqenceId(i)
}

fn next_reg(registers : &mut usize) -> RegIndex {
  let r = RegIndex(*registers);
  *registers += 1;
  r
}

fn push_expr(b : &mut Builder, e : Expr) -> RegIndex{
  let reg = next_reg(b.registers);
  b.ops.push(Op::Expr(reg, e));
  return reg;
}

fn function_call(b : &mut Builder, node : Node) -> RegIndex {
  let function_val = node.children[0];
  let args = &node.children[1..];
  b.arg_values.clear();
  for &arg in args {
    let v = expr_to_value(b, arg);
    b.arg_values.push(v);
  }
  let reg = expr_to_value(b, function_val);
  for (i, value) in b.arg_values.drain(..).enumerate() {
    b.ops.push(Op::Arg{ index: i as u8, value });
  }
  let e = Expr::Invoke(reg);
  push_expr(b, e)
}

fn atom_to_value(b : &mut Builder, node : Node) -> RegIndex {
  let segment = code_segment(b.code, node);
  // boolean literals
  match segment {
    "true" => {
      let e = Expr::LiteralU64(1);
      return push_expr(b, e);
    }
    "false" => {
      let e = Expr::LiteralU64(0);
      return push_expr(b, e);
    }
    _ => (),
  }
  // integer literals
  if let Ok(v) = segment.parse::<u64>() {
    let e = Expr::LiteralU64(v);
    return push_expr(b, e);
  }
  // Look for local
  if let Some(v) = find_local(b, node) {
    return v;
  }
  // Assume global
  let e = Expr::Def(symbols::to_symbol(b.env.st, segment));
  push_expr(b, e)
}

fn list_expr_to_value(b : &mut Builder, node : Node, children : &[Node]) -> RegIndex {
  let head = code_segment(b.code, children[0]);
  // function call
  if head == "call" || head == "ccall" {
    b.arg_values.clear();
    for &arg in &children[2..] {
      let v = expr_to_value(b, arg);
      b.arg_values.push(v);
    }
    let reg = expr_to_value(b, children[1]);
    for (i, value) in b.arg_values.drain(..).enumerate() {
      b.ops.push(Op::Arg{ index: i as u8, value });
    }
    let e = {
      if head == "ccall" {
        let args = children[2..].len();
        Expr::InvokeC(reg, args)
      }
      else {
        Expr::Invoke(reg)
      }
    };
    return push_expr(b, e);
  }
  // operator
  let op : Option<BinOp> = match head {
    "+" => Some(BinOp::Add),
    "-" => Some(BinOp::Sub),
    "*" => Some(BinOp::Mul),
    "/" => Some(BinOp::Div),
    "%" => Some(BinOp::Rem),
    _ => None,
  };
  if let Some(op) = op {
    if let [v1, v2] = children[1..] {
      let v1 = expr_to_value(b, v1);
      let v2 = expr_to_value(b, v2);
      let e = Expr::BinOp(op, v1, v2);
      return push_expr(b, e);
    }
  }
  panic!("unrecognised instruction:\n   {}", code_segment(b.code, node))
}

fn expr_to_value(b : &mut Builder, node : Node) -> RegIndex {
  if node.children.len() == 0 {
    atom_to_value(b, node)
  }
  else {
    list_expr_to_value(b, node, node.children.as_slice())
  }
}

fn find_local(b : &Builder, node : Node)
  -> Option<RegIndex>
{
  let c = to_symbol(b.env.st, b.code, node);
  b.locals.iter()
    .find(|&l| l.name == c).map(|l| l.reg)
}

struct Builder<'l> {
  ops : &'l mut Vec<Op>,
  sequence_info : &'l Vec<SeqInstrs<'l>>,
  locals : &'l mut Vec<LocalVar>,
  registers : &'l mut usize,
  code : &'l str,
  env : &'l Env,

  /// used to store argument values without reallocating a lot
  arg_values : Vec<RegIndex>,
}

fn read_instruction(b : &mut Builder, node : Node) {
  match node_shape(&node, b.code) {
    // set var
    Command("set", [varname, value]) => {
      let var_reg = find_local(b, *varname).expect("no variable found");
      let val_reg = expr_to_value(b, *value);
      b.ops.push(Op::Set(var_reg, val_reg));
    }
    // conditional jump
    Command("cjump", [cond, then_seq, else_seq]) => {
      let cond = expr_to_value(b, *cond);
      let then_seq = to_sequence_id(b.sequence_info, to_symbol(b.env.st, b.code, *then_seq));
      let else_seq = to_sequence_id(b.sequence_info, to_symbol(b.env.st, b.code, *else_seq));
      b.ops.push(Op::CJump{cond, then_seq, else_seq});
    }
    // Jump
    Command("jump", [seq]) => {
      let seq = to_sequence_id(b.sequence_info, to_symbol(b.env.st, b.code, *seq));
      b.ops.push(Op::Jump(seq));
    }
    // Debug
    Command("debug", [v]) => {
      let reg = expr_to_value(b, *v);
      let sym = to_symbol(b.env.st, b.code, *v);
      b.ops.push(Op::Debug(sym, reg));
    }
    // Return
    Command("return", [v]) => {
      let reg = expr_to_value(b, *v);
      b.ops.push(Op::SetReturn(reg));
      b.ops.push(Op::Return);
    }
    Atom("return") => {
      b.ops.push(Op::Return);
    }
    // Assume expression
    _ => {
      expr_to_value(b, node);
    }
  }
}

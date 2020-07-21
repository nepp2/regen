
use crate::{symbols, parse, bytecode};

use symbols::{Symbol, to_symbol};
use parse::{
  AbstractSyntaxTree as AST,
  NodeIndex, node_children, match_head,
};
use bytecode::{
  ByteCode, Op, Expr, RegIndex, codegen
};

use std::collections::HashMap;

pub struct Function {
  pub bytecode : ByteCode
}

type Env = HashMap<Symbol, u64>;

pub fn interpret(ast : &AST) {
  println!("Entering interpreter");
  let children = node_children(ast, AST::root());
  let mut env = Env::new();
  for &c in children {
    if let Some([name, value]) = match_head(ast, c, "def") {
      let name = parse::code_segment(ast, *name);
      let function = codegen_function(ast, *value);
      let value =
        Box::into_raw(Box::new(function)) as u64;
      env.insert(to_symbol(name), value);
    }
  }

  if let Some(&v) = env.get(&to_symbol("main")) {
    let f = unsafe { &*(v as *const Function) };
    let mut stack = [0 ; 2048];
    let mut sf = StackFrame::new(&mut stack);
    sf.interpret(f, &mut env);
    println!("Execution of main function complete.");
  }
  else {
    println!("No main function found.");
  }
}
  
fn codegen_function(ast : &AST, root : NodeIndex) -> Function {
  if let Some([args, body]) = match_head(ast, root, "fun") {
    return Function { bytecode: codegen(ast, *body)};
  }
  panic!("expected function")
}
  
struct StackFrame<'l> {
  /// program counter
  pc : usize,

  /// the stack memory
  stack : &'l mut [u64],
}

impl <'l> StackFrame<'l> {

  fn new(stack : &'l mut [u64]) -> Self {
    StackFrame { pc: 0, stack }
  }

  fn reg(&self, i : RegIndex) -> u64 {
    self.stack[i.0]
  }

  fn reg_mut(&mut self, i : RegIndex) -> &mut u64 {
    &mut self.stack[i.0]
  }

  fn interpret(&mut self, fun : &Function, env : &mut Env) {
    let bc = &fun.bytecode;
    loop {
      match &bc.ops[self.pc] {
        Op::Expr(r, e) => {
          let v = match e {
            Expr::Def(sym) =>
              if let Some(&f) = env.get(&sym) {
                f
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            Expr::LiteralU64(val) =>
              *val as u64,
            Expr::Add(a, b) =>
              self.reg(*a) + self.reg(*b),
          };
          *self.reg_mut(*r) = v;
        }
        Op::Set(var, val) => {
          *self.reg_mut(*var) = self.reg(*val);
        }
        Op::CJump{ cond, then_block, else_block } => {
          if self.reg(*cond) != 0 {
            self.pc = bc.blocks[then_block.0].start_op;
          }
          else {
            self.pc = bc.blocks[else_block.0].start_op;
          }
          continue;
        }
        Op::Jump(block) => {
          self.pc = bc.blocks[block.0].start_op;
          continue;
        }
        Op::Invoke(fun) => {
          let fun = unsafe { &*(self.reg(*fun) as *const Function) };
          let mut frame = StackFrame::new(&mut self.stack[bc.registers..]);
          frame.interpret(fun, env);
        }
        Op::Debug(s) => {
          println!("debug: {}", s);
        }
        Op::Exit => {
          return;
        }
      }
      self.pc += 1;
    }
  }
}

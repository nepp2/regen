
use crate::{symbols, parse, bytecode, env};

use env::Env;
use symbols::to_symbol;
use parse::{
  AbstractSyntaxTree as AST,
  node_children, match_head,
};
use bytecode::{
  Function, Op, Expr, RegIndex,
};

pub fn interpret(ast : &AST) {
  println!("Entering interpreter");
  let children = node_children(ast, AST::root());
  let mut env = Env::new();
  for &c in children {
    if let Some([name, value]) = match_head(ast, c, "def") {
      let name = parse::code_segment(ast, *name);
      let function = bytecode::codegen(&env, ast, *value);
      let value =
        Box::into_raw(Box::new(function)) as u64;
      env.insert(to_symbol(name), value);
    }
  }

  if let Some(&v) = env.get(&to_symbol("main")) {
    let f = unsafe { &*(v as *const Function) };
    let mut stack = [0 ; 2048];
    let mut sf = StackFrame::new(0);
    sf.interpret(&mut stack, f, &mut env);
    println!("Execution of main function complete.");
  }
  else {
    println!("No main function found.");
  }
}
  
struct StackFrame {
  /// program counter
  pc : usize,

  /// the stack base pointer (index of the beginning of the current stack frame)
  sbp : usize,
}

impl StackFrame {

  fn new(sbp : usize) -> Self {
    StackFrame { pc: 0, sbp }
  }

  fn reg(&self, i : RegIndex) -> usize {
    i.0 as usize + self.sbp
  }

  fn interpret(&mut self, stack : &mut [u64], fun : &Function, env : &mut Env) {
    loop {
      let op = &fun.ops[self.pc];
      println!(">>> {}:   {}", self.pc, op);
      match op {
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
              stack[self.reg(*a)] + stack[self.reg(*b)],
          };
          stack[self.reg(*r)] = v;
        }
        Op::Set(var, val) => {
          stack[self.reg(*var)] = stack[self.reg(*val)];
        }
        Op::CJump{ cond, then_block, else_block } => {
          if self.reg(*cond) != 0 {
            self.pc = fun.blocks[then_block.0].start_op;
          }
          else {
            self.pc = fun.blocks[else_block.0].start_op;
          }
          continue;
        }
        Op::Jump(block) => {
          self.pc = fun.blocks[block.0].start_op;
          continue;
        }
        Op::Arg{ index, value } => {
          let index = self.sbp + fun.registers + (*index as usize);
          stack[index] = stack[self.reg(*value)];

        }
        Op::Invoke(f) => {
          let fun_address = stack[self.reg(*f)];
          let f = unsafe { &*(fun_address as *const Function) };
          let mut frame = StackFrame::new(self.sbp + fun.registers);
          frame.interpret(stack, f, env);
        }
        Op::Debug(r) => {
          println!("debug: {}", stack[self.reg(*r)]);
        }
        Op::Exit => {
          return;
        }
      }
      self.pc += 1;
    }
  }
}

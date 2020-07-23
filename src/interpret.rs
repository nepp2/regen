
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
    let mut p = ProcessorState{ pc: 0, sbp: 0 };
    interpreter_loop(&mut stack, &mut p, f, &mut env);
    println!("Execution of main function complete.");
  }
  else {
    println!("No main function found.");
  }
}
  
struct ProcessorState {
  /// program counter
  pc : usize,

  /// the stack base pointer (index of the beginning of the current stack frame)
  sbp : usize,
}

fn reg(p : &ProcessorState, i : RegIndex) -> usize {
  i.0 as usize + p.sbp
}

fn interpreter_loop(stack : &mut [u64], p : &mut ProcessorState, fun : &Function, env : &mut Env) {
  loop {
    let op = fun.ops[p.pc];
    println!(">>> {}:   {}", p.pc, op);
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
            val as u64,
          Expr::Add(a, b) =>
            stack[reg(p, a)] + stack[reg(p, b)],
        };
        stack[reg(p, r)] = v;
      }
      Op::Set(var, val) => {
        stack[reg(p, var)] = stack[reg(p, val)];
      }
      Op::CJump{ cond, then_block, else_block } => {
        if reg(p, cond) != 0 {
          p.pc = fun.blocks[then_block.0].start_op;
        }
        else {
          p.pc = fun.blocks[else_block.0].start_op;
        }
        continue;
      }
      Op::Jump(block) => {
        p.pc = fun.blocks[block.0].start_op;
        continue;
      }
      Op::Arg{ index, value } => {
        let index = p.sbp + fun.registers + (index as usize);
        stack[index] = stack[reg(p, value)];

      }
      Op::Invoke(f) => {
        let fun_address = stack[reg(p, f)];
        let f = unsafe { &*(fun_address as *const Function) };
        let mut next_p = ProcessorState{ pc: 0, sbp: p.sbp + fun.registers };
        interpreter_loop(stack, &mut next_p, f, env);
      }
      Op::Debug(r) => {
        println!("debug: {}", stack[reg(p, r)]);
      }
      Op::Exit => {
        return;
      }
    }
    p.pc += 1;
  }
}

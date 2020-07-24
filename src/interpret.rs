
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
    let f = v as *const Function;
    let mut stack = [0 ; 2048];
    let mut shadow_stack = vec![Frame { pc: 0, sbp: 0, f }];
    interpreter_loop(&mut stack, &mut shadow_stack, &mut env);
    println!("Execution of main function complete.");
  }
  else {
    println!("No main function found.");
  }
}
  
struct Frame {
  /// program counter
  pc : usize,

  /// the stack base pointer (index of the beginning of the current stack frame)
  sbp : usize,

  /// function pointer
  f : *const Function,
}

fn reg(sbp : usize, i : RegIndex) -> usize {
  i.0 as usize + sbp
}

fn interpreter_loop(stack : &mut [u64], shadow_stack : &mut Vec<Frame>, env : &mut Env) {
  let mut frame = shadow_stack.pop().unwrap();
  let mut return_addr = 0;
  'outer: loop {
    let fun = unsafe { &*frame.f };
    let sbp = frame.sbp;
    loop {
      let op = fun.ops[frame.pc];
      // TODO: println!(">>> {}:   {}", frame.pc, op);
      match op {
        Op::Expr(r, e) => {
          let r = reg(sbp, r);
          match e {
            Expr::Def(sym) =>
              if let Some(&f) = env.get(&sym) {
                stack[r] = f;
              }
              else {
                panic!("Symbol '{}' not present in env", sym);
              }
            Expr::LiteralU64(val) => {
              stack[r] = val as u64;
            }
            Expr::Add(a, b) => {
              stack[r] = stack[reg(sbp, a)] + stack[reg(sbp, b)];
            }
            Expr::Invoke(f) => {
              let fun_address = stack[reg(sbp, f)];
              let f = unsafe { &*(fun_address as *const Function) };
              // advance the current frame past the call, and store it on the
              // shadow stack to be returned to later
              frame.pc += 1;
              shadow_stack.push(frame);
              // set the new frame
              frame = Frame{ pc: 0, sbp: sbp + fun.registers, f };
              // set the return address
              return_addr = r;
              break;
            }
          };
        }
        Op::Set(var, val) => {
          stack[reg(sbp, var)] = stack[reg(sbp, val)];
        }
        Op::SetReturn(val) => {
          stack[return_addr] = stack[reg(sbp, val)];
        }
        Op::CJump{ cond, then_block, else_block } => {
          if reg(sbp, cond) != 0 {
            frame.pc = fun.blocks[then_block.0].start_op;
          }
          else {
            frame.pc = fun.blocks[else_block.0].start_op;
          }
          continue;
        }
        Op::Jump(block) => {
          frame.pc = fun.blocks[block.0].start_op;
          continue;
        }
        Op::Arg{ index, value } => {
          let index = frame.sbp + fun.registers + (index as usize);
          stack[index] = stack[reg(sbp, value)];

        }
        Op::Debug(r) => {
          println!("debug: {}", stack[reg(sbp, r)]);
        }
        Op::Return => {
          if let Some(prev_frame) = shadow_stack.pop() {
            frame = prev_frame;
            break;
          }
          else {
            break 'outer;
          }
        }
      }
      frame.pc += 1;
    }
  }
  shadow_stack.push(frame);
}

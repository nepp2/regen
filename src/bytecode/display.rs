
use super::definition::*;

use std::fmt;

impl fmt::Display for BytecodeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      writeln!(f, )?;
      for (i, b) in self.blocks.iter().enumerate() {
        writeln!(f, "Block {}: {}", i, b.name)?;
        let end = b.start_op + b.num_ops;
        for (i, op) in self.ops[b.start_op..end].iter().enumerate() {
          let i = b.start_op + i;
          write!(f, "   {}: ", i)?;
          writeln!(f, "{:?}", op)?;
        }
      }
      Ok(())
    }
  }
  
  impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      use BinOp::*;
      match self {
        Add => write!(f, "+"),
        Sub => write!(f, "-"),
        Mul => write!(f, "*"),
        Div => write!(f, "/"),
        Rem => write!(f, "%"),
      }
    }
  }
  
  impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        Expr::LiteralU64(v) =>
          write!(f, "{}", v)?,
        Expr::Def(sym) =>
          write!(f, "{}", sym)?,
        Expr::BinOp(op, a, b) =>
          write!(f, "reg[{}] {} reg[{}]", a.0, op, b.0)?,
        Expr::Invoke(reg) =>
          write!(f, "Invoke reg[{}]", reg.0)?,
        Expr::InvokeC(reg, args) =>
          write!(f, "InvokeC reg[{}] ({} args)", reg.0, args)?,
      }
      Ok(())
    }
  }
  
  impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        Op::Expr(reg, expr) =>
          write!(f, "reg[{}] = {}", reg.0, expr)?,
        Op::Set(a, b) =>
          write!(f, "reg[{}] := reg[{}]", a.0, b.0)?,
        Op::SetReturn(v) =>
          write!(f, "RetVal := reg[{}]", v.0)?,
        Op::CJump{ cond, then_block, else_block } =>
          write!(f, "CJump reg[{}] to block[{}] else block[{}]",
            cond.0, then_block.0, else_block.0)?,
        Op::Debug(reg) =>
          write!(f, "Debug reg[{}]", reg.0)?,
        Op::Jump(block) =>
          write!(f, "Jump to block[{}]", block.0)?,
        Op::Arg{ index, value } =>
          write!(f, "Arg {} = reg[{}]", index, value.0)?,
        Op::Return =>
          write!(f, "Return")?,
      }
      Ok(())
    }
  }

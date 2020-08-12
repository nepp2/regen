
use super::definition::*;

use std::fmt;

impl fmt::Display for BytecodeFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Args:")?;
    for (a, i) in &self.locals.as_slice()[..self.args] {
      write!(f, " {} ({}),", a, i.0)?;
    }
    writeln!(f)?;
    write!(f, "Locals:")?;
    for (a, i) in &self.locals.as_slice()[self.args..] {
      write!(f, " {} ({}),", a, i.0)?;
    }
    writeln!(f)?;
    writeln!(f, "Registers: {}", self.registers)?;
    for (i, b) in self.sequences.iter().enumerate() {
      writeln!(f, "Sequence {} ({}):", i, b.name)?;
      let end = b.start_op + b.num_ops;
      for op in self.ops[b.start_op..end].iter() {
        writeln!(f, "   {}", op)?;
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
        write!(f, "let {} = {}", reg, expr)?,
      Op::Set(a, b) =>
        write!(f, "set reg[{}] = reg[{}]", a, b)?,
      Op::SetReturn(v) =>
        write!(f, "set RetVal = reg[{}]", v)?,
      Op::CJump{ cond, then_seq, else_seq } =>
        write!(f, "CJump {} to seq[{}] else seq[{}]",
          cond, then_seq.0, else_seq.0)?,
      Op::Debug(reg) =>
        write!(f, "Debug reg[{}]", reg)?,
      Op::Jump(seq) =>
        write!(f, "Jump to seq[{}]", seq.0)?,
      Op::Arg{ index, value } =>
        write!(f, "set Arg {} = reg[{}]", index, value)?,
      Op::Return =>
        write!(f, "Return")?,
    }
    Ok(())
  }
}

impl fmt::Display for RegIndex {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "${}", self.0)
  }
}

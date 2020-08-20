
use super::definition::*;

use std::fmt;

impl fmt::Display for BytecodeFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Args:")?;
    for l in &self.locals.as_slice()[..self.args] {
      write!(f, " {} ({}),", l.name, l.reg)?;
    }
    writeln!(f)?;
    write!(f, "Locals:")?;
    for l in &self.locals.as_slice()[self.args..] {
      write!(f, " {} ({}),", l.name, l.reg)?;
    }
    writeln!(f)?;
    writeln!(f, "Registers: {}", self.registers)?;
    for (i, b) in self.sequence_info.iter().enumerate() {
      writeln!(f, "Sequence {} ({}):", i, b.name)?;
      let end = b.start_op + b.num_ops;
      for op in self.ops[b.start_op..end].iter() {
        writeln!(f, "   {}", op)?;
      }
    }
    Ok(())
  }
}
  
impl fmt::Display for Operator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Operator::*;
    match self {
      Add => write!(f, "+"),
      Sub => write!(f, "-"),
      Mul => write!(f, "*"),
      Div => write!(f, "/"),
      Rem => write!(f, "%"),
      Eq => write!(f, "="),
      LT => write!(f, "<"),
      GT => write!(f, ">"),
      LTE => write!(f, "<="),
      GTE => write!(f, ">="),
      Load(a) => write!(f, "load_{}", a),
      Ref => write!(f, "ref"),
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
      Expr::BinaryOp(op, a, b) =>
        write!(f, "{} {} {}", a, op, b)?,
      Expr::UnaryOp(op, a) =>
        write!(f, "{} {}", op, a)?,
      Expr::Invoke(reg) =>
        write!(f, "Invoke {}", reg)?,
      Expr::InvokeC(reg, args) =>
        write!(f, "InvokeC {} ({} args)", reg, args)?,
    }
    Ok(())
  }
}

impl fmt::Display for Alignment {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Alignment::*;
    write!(f, "{}", match self {
      U64 => "u64", U32 => "u32", U16 => "u16", U8 => "u8", 
    })
  }
}

impl fmt::Display for Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Op::Expr(reg, expr) =>
        write!(f, "let {} = {}", reg, expr)?,
      Op::Set(a, b) =>
        write!(f, "set {} = {}", a, b)?,
      Op::Store{ alignment, pointer, value } =>
        write!(f, "mem[{}] = {} ({})", pointer, value, alignment)?,
      Op::SetReturn(v) =>
        write!(f, "set RetVal = {}", v)?,
      Op::CJump{ cond, then_seq, else_seq } =>
        write!(f, "CJump {} to seq[{}] else seq[{}]",
          cond, then_seq.0, else_seq.0)?,
      Op::Debug(sym, reg) =>
        write!(f, "Debug {}: {}", sym, reg)?,
      Op::Jump(seq) =>
        write!(f, "Jump to seq[{}]", seq.0)?,
      Op::Arg{ index, value } =>
        write!(f, "set Arg {} = {}", index, value)?,
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

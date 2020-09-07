/// Display functions for bytecode

use super::definition::*;

use std::fmt;



impl fmt::Display for BytecodeFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, ";; Frame size: {}", self.frame_bytes)?;
    write!(f, "(fun (")?;
    for v in &self.locals.as_slice()[..self.args] {
      write!(f, " (u{} ${})", v.var.bytes * 8, v.name)?;
    }
    writeln!(f, ") (")?;
    let locals = &self.locals.as_slice()[self.args..];
    if locals.len() > 0 {
      write!(f, " (locals")?;
      for v in locals {
        write!(f, " (u{} ${})", v.var.bytes * 8, v.name)?;
      }
      writeln!(f, ")")?;
    }
    for b in self.sequence_info.iter() {
      writeln!(f, "  (seq {} (", b.name)?;
      let end = b.start_op + b.num_ops;
      for op in self.ops[b.start_op..end].iter() {
        write!(f, "    {}", self.d(op))?;
        writeln!(f)?;
      }
      writeln!(f, "  ))")?;
    }
    write!(f, "))")?;
    Ok(())
  }
}

struct BytecodeDisplay<'l, X> {
   bc : &'l BytecodeFunction,
   x : &'l X,
}


impl BytecodeFunction {
  fn d<'l, X>(&'l self, x : &'l X) -> BytecodeDisplay<'l, X> {
     BytecodeDisplay { bc: self, x }
  }
}

fn display_var(f: &mut fmt::Formatter<'_>, var : FrameVar, b : &BytecodeFunction) -> fmt::Result {
  if let Some(l) = b.locals.iter().find(|l| l.var.id == var.id) {
    write!(f, "${}", l.name)
  }
  else {
    write!(f, "${}", var.id)
  }
}

impl <'l> fmt::Display for BytecodeDisplay<'l, FrameVar> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(l) = self.bc.locals.iter().find(|l| l.var.id == self.x.id) {
      write!(f, "${}", l.name)
    }
    else {
      write!(f, "${}", self.x.id)
    }
  }
}

impl <'l> fmt::Display for BytecodeDisplay<'l, SequenceId> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.bc.sequence_info[self.x.0].name)
  }
}

impl <'l> fmt::Display for BytecodeDisplay<'l, Op> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let bc = self.bc;
    match self.x {
      Op::Expr(reg, expr) =>
        write!(f, "(let {} {})", bc.d(reg), bc.d(expr))?,
      Op::Set(a, b) =>
        write!(f, "(set {} (u64 {}))", bc.d(a), bc.d(b))?,
      Op::Store{ byte_width, pointer, value } =>
        write!(f, "(store {} (u{} {}) )", bc.d(pointer), byte_width, bc.d(value))?,
      Op::SetReturn(v) =>
        write!(f, "(set RetVal (u64 {}))", bc.d(v))?,
      Op::CJump{ cond, then_seq, else_seq } =>
        write!(f, "(cjump {} {} {})",
          bc.d(cond), bc.d(then_seq), bc.d(else_seq))?,
      Op::Debug(sym, reg) =>
        write!(f, "(debug {} {})", sym, bc.d(reg))?,
      Op::Jump(seq) =>
        write!(f, "(jump {})", bc.d(seq))?,
      Op::Arg{ byte_offset, value } =>
        write!(f, "(set (+ Arg {}) (u64 {}))", byte_offset, bc.d(value))?,
      Op::Return =>
        write!(f, "return")?,
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

impl <'l> fmt::Display for BytecodeDisplay<'l, Expr> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let bc = self.bc;
    match self.x {
      Expr::LiteralU64(v) =>
        write!(f, "{}", v)?,
      Expr::Def(sym) =>
        write!(f, "{}", sym)?,
      Expr::BinaryOp(op, a, b) =>
        write!(f, "({} {} {})", op, bc.d(a), bc.d(b))?,
      Expr::UnaryOp(op, a) =>
        write!(f, "({} {})", op, bc.d(a))?,
      Expr::Invoke(reg) =>
        write!(f, "(call {})", bc.d(reg))?,
      Expr::InvokeC(reg, args) =>
        write!(f, "(ccall {} (arg_count {}))", bc.d(reg), args)?,
    }
    Ok(())
  }
}

impl fmt::Display for ByteWidth {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ByteWidth::*;
    write!(f, "{}", match self {
      U64 => "64", U32 => "32", U16 => "16", U8 => "8", 
    })
  }
}

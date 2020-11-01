/// Display functions for bytecode

use super::definition::*;

use std::fmt;

impl fmt::Display for FunctionBytecode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, ";; Frame size: {}", self.frame_bytes)?;
    write!(f, ";; Frame vars:")?;
    for v in self.locals.as_slice() {
      writeln!(f)?;
      write!(f, "  {}: {}", self.d(&v.id), v.t)?;
    }
    writeln!(f)?;
    write!(f, "(fun (")?;
    for v in &self.locals.as_slice()[..self.args] {
      write!(f, " ({} {})", v.t, self.d(&v.id))?;
    }
    writeln!(f, ") (")?;
    for b in self.sequence_info.iter() {
      writeln!(f, "  (seq {} (", b.name)?;
      let end = b.start_instruction + b.num_instructions;
      for instr in self.instrs[b.start_instruction..end].iter() {
        write!(f, "    {}", self.d(instr))?;
        writeln!(f)?;
      }
      writeln!(f, "  ))")?;
    }
    write!(f, "))")?;
    Ok(())
  }
}

struct BytecodeDisplay<'l, X> {
   bc : &'l FunctionBytecode,
   x : &'l X,
}


impl FunctionBytecode {
  fn d<'l, X>(&'l self, x : &'l X) -> BytecodeDisplay<'l, X> {
     BytecodeDisplay { bc: self, x }
  }
}

impl <'l> fmt::Display for BytecodeDisplay<'l, LocalId> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(n) = self.bc.locals[self.x.id].name {
      write!(f, "${}", n)
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

impl <'l> fmt::Display for BytecodeDisplay<'l, Instr> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let bc = self.bc;
    match self.x {
      Instr::Expr(reg, expr) =>
        write!(f, "(let {} {})",
          bc.d(reg),
          bc.d(expr))?,
      Instr::Store{ pointer, value } =>
        write!(f, "(store {} {})",
          bc.d(pointer),
          bc.d(value))?,
      Instr::CJump{ cond, then_seq, else_seq } =>
        write!(f, "(cjump {} {} {})",
          bc.d(cond), bc.d(then_seq), bc.d(else_seq))?,
      Instr::Debug(sym, reg, _) =>
        write!(f, "(debug {} {})", sym, bc.d(reg))?,
      Instr::Jump(seq) =>
        write!(f, "(jump {})", bc.d(seq))?,
      Instr::Return(Some(v)) =>
        write!(f, "(return ({} {}))", bc.locals[v.id].t, bc.d(v))?,
      Instr::Return(None) =>
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
      Not => write!(f, "!"),
    }
  }
}

impl <'l> fmt::Display for BytecodeDisplay<'l, Expr> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let bc = self.bc;
    match self.x {
      Expr::LiteralU64(v) =>
        write!(f, "{}", v),
      Expr::Def(sym) =>
        write!(f, "{}", sym),
      Expr::LocalAddr(local) =>
        write!(f, "(ref {})", bc.d(local)),
      Expr::Init(t, fields) => {
        write!(f, "(init {} ", t)?;
        for fv in fields.as_slice() {
          write!(f, "{} ", bc.d(fv))?;
        }
        write!(f, ")")
      }
      Expr::FieldIndex{ tuple_addr, index } =>
        write!(f, "(. {} {})", bc.d(tuple_addr), index),
      Expr::BinaryOp(op, a, b) =>
        write!(f, "({} {} {})", op, bc.d(a), bc.d(b)),
      Expr::UnaryOp(op, a) =>
        write!(f, "({} {})", op, bc.d(a)),
      Expr::Invoke(reg, args) => {
        write!(f, "(call {} ", bc.d(reg))?;
        for arg in args.as_slice() {
          write!(f, "{} ", bc.d(arg))?;
        }
        write!(f, ")")
      }
      Expr::InvokeC(reg, args) => {
        write!(f, "(ccall {} ", bc.d(reg))?;
        for arg in args.as_slice() {
          write!(f, "{} ", bc.d(arg))?;
        }
        write!(f, ")")
      }
      Expr::Load(ptr) =>
        write!(f, "(load u{} {})",
          bc.locals[ptr.id].t,
          bc.d(ptr)),
    }
  }
}

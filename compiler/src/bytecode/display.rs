/// Display functions for bytecode

use super::definition::*;

use std::fmt;

use crate::types::TypeHandle;

impl fmt::Display for FunctionBytecode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, ";; Frame size: {}", self.frame_bytes)?;
    write!(f, ";; Frame vars:")?;
    for v in self.locals.as_slice() {
      writeln!(f)?;
      write!(f, "  {}: {}", v, v.t)?;
    }
    writeln!(f)?;
    write!(f, "(fun (")?;
    for v in &self.locals.as_slice()[..self.args] {
      write!(f, " ({} {})", v, v.t)?;
    }
    writeln!(f, ") (")?;
    for b in self.sequence_info.iter() {
      writeln!(f, "  (seq {} (", b.name)?;
      let end = b.start_instruction + b.num_instructions;
      for instr in self.instrs[b.start_instruction..end].iter() {
        write!(f, "    {}", instr)?;
        writeln!(f)?;
      }
      writeln!(f, "  ))")?;
    }
    write!(f, "))")?;
    Ok(())
  }
}

struct DisplayExpr(TypeHandle, InstrExpr);

impl fmt::Display for LocalInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(n) = self.name {
      write!(f, "${}", n)
    }
    else {
      write!(f, "${}", self.index)
    }
  }
}

impl fmt::Display for SequenceHandle {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}

impl <'l> fmt::Display for Instr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Instr::Expr(reg, expr) => {
        let t = reg.t;
        write!(f, "(let {} {})", reg, DisplayExpr(t, *expr))?
      }
      Instr::Store{ pointer, value } =>
        write!(f, "(store {} {})", pointer, value)?,
      Instr::CJump{ cond, then_seq, else_seq } =>
        write!(f, "(cjump {} {} {})", cond, then_seq, else_seq)?,
      Instr::Debug(sym, reg, _) =>
        write!(f, "(debug {} {})", sym, reg)?,
      Instr::Jump(seq) =>
        write!(f, "(jump {})", seq)?,
      Instr::Return(Some(v)) =>
        write!(f, "(return ({} {}))", v.t, v)?,
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
      Eq => write!(f, "=="),
      NEq => write!(f, "!="),
      LT => write!(f, "<"),
      GT => write!(f, ">"),
      LTE => write!(f, "<="),
      GTE => write!(f, ">="),
      Not => write!(f, "!"),
      And => write!(f, "&&"),
      Or => write!(f, "||"),
      BitwiseNot => write!(f, "~"),
      BitwiseAnd => write!(f, "&"),
      BitwiseOr => write!(f, "|"),
    }
  }
}

impl fmt::Display for DisplayExpr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.1 {
      InstrExpr::LiteralI64(v) =>
        write!(f, "{}", v),
      InstrExpr::StaticValue(t, _v) =>
        write!(f, "(static_value {})", t),
      InstrExpr::LocalAddr(local) =>
        write!(f, "(ref {})", local),
      InstrExpr::Array(elements) => {
        write!(f, "(array {} ", self.0)?;
        for ev in elements.as_slice() {
          write!(f, "{} ", ev)?;
        }
        write!(f, ")")
      }
      InstrExpr::Init(fields) => {
        write!(f, "(init {} ", self.0)?;
        for fv in fields.as_slice() {
          write!(f, "{} ", fv)?;
        }
        write!(f, ")")
      }
      InstrExpr::ZeroInit => write!(f, "zero_init"),
      InstrExpr::FieldIndex{ struct_addr, index } =>
        write!(f, "(. {} {})", struct_addr, index),
      InstrExpr::BinaryOp(op, a, b) =>
        write!(f, "({} {} {})", op, a, b),
      InstrExpr::UnaryOp(op, a) =>
        write!(f, "({} {})", op, a),
      InstrExpr::Invoke(reg, args) => {
        write!(f, "(call {} ", reg)?;
        for arg in args.as_slice() {
          write!(f, "{} ", arg)?;
        }
        write!(f, ")")
      }
      InstrExpr::InvokeC(reg, args) => {
        write!(f, "(ccall {} ", reg)?;
        for arg in args.as_slice() {
          write!(f, "{} ", arg)?;
        }
        write!(f, ")")
      }
      InstrExpr::Load(ptr) =>
        write!(f, "(load {} {})", ptr.t, ptr),
      InstrExpr::PtrOffset {ptr, offset } => {
        write!(f, "(ptr_offset {} {})", ptr, offset)
      }
      InstrExpr::Cast(v) =>
        write!(f, "(cast {})", v),
    }
  }
}

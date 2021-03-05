
use std::fmt;

use crate::{perm_alloc::SlicePtr, parse::SrcLocation};

/// Returns an error that isn't wrapped in Result::Err
pub fn error_raw<L : Into<SrcLocation>, S : Into<ErrorContent>>(loc : L, message : S) -> Error {
  Error { message: message.into(), location: loc.into() }
}

/// Returns an error wrapped in Result::Err
pub fn error<T, L : Into<SrcLocation>, S : Into<ErrorContent>>(loc : L, message : S) -> Result<T, Error> {
  Err(Error { message: message.into(), location: loc.into() })
}

impl <'l> Into<SrcLocation> for &'l SrcLocation {
  fn into(self) -> SrcLocation {
    *self
  }
}

impl <S : Into<String>> From<S> for ErrorContent {
  fn from(s : S) -> ErrorContent {
    let s : String = s.into();
    ErrorContent::Message(s)
  }
}

pub enum ErrorContent {
  Message(String),
  InnerErrors(String, SlicePtr<Error>),
}

pub struct Error {
  pub message : ErrorContent,
  pub location : SrcLocation,
}

impl Error {
  pub fn display(&self) -> ErrorDisplay {
    ErrorDisplay{ e: self }
  }
}

impl <'l> fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.display())
  }
}

pub struct ErrorDisplay<'l> {
  e : &'l Error,
}

impl <'l> fmt::Display for ErrorDisplay<'l> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use ansi_term::Colour::RGB;
    let red = RGB(255, 100, 100);
    write!(f, "{}", red.prefix())?;
    match &self.e.message {
      ErrorContent::Message(m) => {
        write!(f, "error: {} ({})", m, self.e.location)?;
      },
      ErrorContent::InnerErrors(m, es) => {
        writeln!(f, "error: {} ({})", m, self.e.location)?;
        writeln!(f, "  inner errors:")?;
        for e in es.as_slice() {
          writeln!(f, "    {}", e.display())?
        }
      },
    }
    write!(f, "{}", red.suffix())?;
    Ok(())
  }
}

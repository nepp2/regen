
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
  pub fn display(&self) -> UnsourcedError {
    UnsourcedError{ e: self }
  }
}

impl <'l> fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.display())
  }
}

pub struct UnsourcedError<'l> {
  e : &'l Error,
}

impl <'l> fmt::Display for UnsourcedError<'l> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.e.location)?;
    match &self.e.message {
      ErrorContent::Message(m) => {
        write!(f, ", message: {}", m)
      },
      ErrorContent::InnerErrors(m, es) => {
        writeln!(f, ", message: {}", m)?;
        writeln!(f, "  inner errors:")?;
        for e in es.as_slice() {
          writeln!(f, "    {}", e.display())?
        }
        Ok(())
      },
    }
  }
}

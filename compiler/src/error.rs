
use std::fmt;

use crate::{parse::SrcLocation};

/// Returns an error that isn't wrapped in Result::Err
pub fn error<L, S>(loc : L, message : S) -> Error
  where L : Into<SrcLocation>, S : Into<String>
{
  Error { message: message.into(), location: loc.into() }
}

/// Returns an error wrapped in Result::Err
pub fn err<T, L, S>(loc : L, message : S) -> Result<T, Error>
  where L : Into<SrcLocation>, S : Into<String>
{
  Err(error(loc, message))
}

impl <'l> Into<SrcLocation> for &'l SrcLocation {
  fn into(self) -> SrcLocation {
    *self
  }
}

pub struct Error {
  pub message : String,
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
    write!(f, "error: {} ({})", self.e.message, self.e.location)?;
    write!(f, "{}", red.suffix())?;
    Ok(())
  }
}

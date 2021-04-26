
use std::{fmt};

use crate::{error::{Error, error}, perm_alloc::Ptr};
use super::expr::{CodeModule, SrcLocation};

const SYNTAX : &'static [&'static str] =
  &["==", "!=", "<=", ">=", "=>", "+=",
    "-=", "*=", "/=", "||", "&&", "::",
    
    "{", "}", "(", ")", "[", "]", "<", ">", ";", ":",
    ",", ".", "=", "+", "-", "*", "/", "%", "?", "|",
    "&", "^", "!", "$", "'", "#"];

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
  Symbol, FloatLiteral, IntLiteral,
  StringLiteral, Whitespace, Newline, Comment
}

use TokenType::*;

#[derive(Clone)]
pub struct Token<'l> {
  pub string : &'l str,
  pub token_type : TokenType,
  pub loc : SrcLocation,
}

impl <'l> fmt::Display for Token<'l> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}({})", self.token_type, self.string)
  }
}

impl <'l> Token<'l> {
  pub fn literal(&self) -> Option<&str> {
    match self.token_type {
      Symbol | Whitespace | Newline | Comment => None,
      FloatLiteral | IntLiteral | StringLiteral
        => Some(&self.string)
    }
  }

  pub fn can_ignore(&self) -> bool {
    match self.token_type {
      Whitespace | Newline | Comment => true,
      Symbol | FloatLiteral | IntLiteral | StringLiteral => false,
    }
  }

  pub fn to_string(&self) -> String {
    self.string.to_string()
  }
}

pub struct TokenIterator<'l> {
  code : &'l str,
  start_pos : usize,
  pos : usize,
  module : Ptr<CodeModule>,
}

impl <'l> TokenIterator<'l> {

  fn new(code : &'l str, module : Ptr<CodeModule>) -> TokenIterator<'l> {
    TokenIterator {
      code,
      start_pos: 0,
      pos: 0,
      module,
    }
  }

  fn has_chars(&self) -> bool {
    self.code[self.pos..].chars().next().is_some()
  }

  fn peek(&self) -> char {
    self.code[self.pos..].chars().next().unwrap()
  }

  fn next_char_pos(&self) -> usize {
    let mut cs = self.code[self.pos..].char_indices();
    // skip the character at position zero
    cs.next().unwrap();
    // the next character's byte index is the width of the first character
    if let Some((char_width, _)) = cs.next() {
      self.pos + char_width
    }
    else {
      // if there is no next character, this is the end of the string
      self.code.len()
    }
  }

  fn append_char(&mut self) {
    self.pos = self.next_char_pos();
  }

  fn try_append_char(&mut self, c : char) -> bool {
    if self.peek() == c {
      self.append_char();
      true
    }
    else { 
      false
    }
  }

  fn get_src_location(&mut self) -> SrcLocation {
    SrcLocation { start: self.start_pos, end: self.pos, module: self.module }
  }

  fn complete_token(&mut self, token_type : TokenType) -> Token<'l> {
    self.complete_token_with_string_range(token_type, self.start_pos, self.pos)
  }

  fn complete_token_with_string_range(&mut self, token_type : TokenType, start : usize, end : usize) -> Token<'l> {
    let loc = self.get_src_location();
    let t = Token {
      string: &self.code[start..end],
      token_type: token_type,
      loc : loc,
    };
    self.start_pos = self.pos;
    t
  }

  fn raise_error(&mut self, message : String) -> Error {
    let location = self.get_src_location();
    self.start_pos = self.pos;
    error(location, message)
  }

  fn lex_newline(&mut self) -> Option<Token<'l>> {
    if self.try_append_char('\n') {
      Some(self.complete_token(Newline))
    }
    else if self.append_string("\r\n") {
      Some(self.complete_token(Newline))
    }
    else { None }
  }

  fn is_number(&self) -> bool {
    let c = self.peek();
    c >= '0' && c <= '9'
  }

  fn iter_char_while<C, O>(&mut self, condition : C, mut operation : O)
    where C : Fn(&TokenIterator<'l>) -> bool, O : FnMut(&mut TokenIterator<'l>)
  {
    while self.has_chars() {
      if condition(self) {
        operation(self);
      }
      else{
        break;
      }
    }
  }

  fn append_char_while<C>(&mut self, condition : C)
    where C : Fn(&TokenIterator<'l>) -> bool
  {
    self.iter_char_while(condition, &mut |ti : &mut TokenIterator| { ti.append_char() });
  }

  fn lex_number(&mut self) -> Option<Result<Token<'l>, Error>> {
    if self.is_number() {
      self.append_char_while(&TokenIterator::is_number);
      let literal_type =
        if self.has_chars() && self.peek() == '.' {
          self.append_char();
          self.append_char_while(&TokenIterator::is_number);
          TokenType::FloatLiteral
        }
        else {
          TokenType::IntLiteral
        };
      if self.has_chars() && self.is_symbol_start_char() {
        self.append_char_while(&TokenIterator::is_symbol_middle_char);
        Some(Err(self.raise_error("Malformed literal".to_string())))
      }
      else{
        Some(Ok(self.complete_token(literal_type)))
      }
    }
    else { None }
  }

  fn is_symbol_start_char(&self) -> bool {
    let c = self.peek();
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  }

  fn is_symbol_middle_char(&self) -> bool {
    self.is_symbol_start_char() || {
      let c = self.peek();
      c >= '0' && c <= '9'
    }
  }

  fn lex_symbol(&mut self) -> Option<Token<'l>> {
    if self.is_symbol_start_char() {
      self.append_char();
      self.append_char_while (&TokenIterator::is_symbol_middle_char);
      Some(self.complete_token(TokenType::Symbol))
    }
    else { None }
  }

  /// returns true for a single space or tab (not for newline characters)
  fn is_space_char(&self) -> bool {
    let c = self.peek();
    c == '\t' || c == ' '
  }

  fn lex_space(&mut self) -> Option<Token<'l>> {
    if self.is_space_char() {
      self.append_char_while(&TokenIterator::is_space_char);
      Some(self.complete_token(Whitespace))
    }
    else { None }
  }

  fn peek_string(&self, s : &str) -> bool {
    let p = self.pos;
    if self.pos + s.len() > self.code.len() {
      return false;
    }
    let slice = &self.code[p..p + s.len()];
    return slice == s;
  }

  fn append_string(&mut self, s : &str) -> bool {
    if self.peek_string(s) {
      self.pos += s.len();
      return true;
    }
    return false;
  }

  fn unknown_token(&mut self) -> Error {
    self.append_char();
    self.raise_error("Unknown token".to_string())
  }

  fn lex_comment(&mut self) -> Option<Token<'l>> {
    if self.peek_string("//") {
      self.append_char_while(&|ti : &TokenIterator| {
        let c = ti.peek();
        c != '\n'
      });
      return Some(self.complete_token(Comment));
    }
    return None;
  }

  fn lex_syntax(&mut self) -> Option<Token<'l>> {
    // TODO: this is slow
    for s in SYNTAX {
      if self.append_string(s) {
        return Some(self.complete_token(TokenType::Symbol));
      }
    }
    return None;
  }

  fn lex_string_literal(&mut self) -> Option<Result<Token<'l>, Error>> {
    if self.peek() != '"' {
      return None;
    }
    // carefully includes the quotes in the src location, but not in the string itself
    self.append_char();
    let string_start = self.pos;
    let mut string_end;
    loop {
      if !self.has_chars() {
        return Some(Err(self.raise_error("malformed string literal".to_string())));
      }
      let c = self.peek();
      // TODO: this needs to be fixed. perhaps this processing should happen
      // in the parser, when the string literal is created.
      //
      // if c == '\\' {
      //   // slash pattern, e.g. \n for newline
      //   self.skip_char();
      //   let c = self.peek();
      //   match c {
      //     '\\' => self.current_token.push('\\'),
      //     'n' => self.current_token.push('\n'),
      //     't' => self.current_token.push('\t'),
      //     '"' => self.current_token.push('"'),
      //     '0' => self.current_token.push('\0'),
      //     _ => return Err(self.raise_error(start_pos, format!("unexpected pattern '\\{}' in string literal", c))),
      //   }
      //   self.skip_char();
      // }
      string_end = self.pos;
      self.append_char();
      if c == '"' { break; }
    }
    Some(Ok(self.complete_token_with_string_range(StringLiteral, string_start, string_end)))
  }
}

impl <'l> Iterator for TokenIterator<'l> {
  type Item = Result<Token<'l>, Error>;
  
  fn next(&mut self) -> Option<Result<Token<'l>, Error>> {
    if !self.has_chars() { return None }
    let t = {
      if let Some(t) = self.lex_newline() { Ok(t) }
      else if let Some(t) = self.lex_space() { Ok(t) }
      else if let Some(t) = self.lex_symbol() { Ok(t) }
      else if let Some(r) = self.lex_string_literal() { r }
      else if let Some(r) = self.lex_number() { r }
      else if let Some(t) = self.lex_comment() { Ok(t) }
      else if let Some(t) = self.lex_syntax() { Ok(t) }
      else { Err(self.unknown_token()) }
    };
    Some(t)
  }
}

pub fn lex<'l>(module : &'l Ptr<CodeModule>) -> Result<Vec<Token<'l>>, Error> {
  let code = module.code.as_str();
  let ti = TokenIterator::new(code, *module);
  let mut tokens = vec![];
  for rt in ti {
    let t = rt?;
    if !t.can_ignore() {
      tokens.push(t);
    }
  }
  Ok(tokens)
}

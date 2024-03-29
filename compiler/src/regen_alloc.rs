use std::{fmt, ops::Range};
use std::ops::{Deref, DerefMut, Index, RangeFrom};
use core::hash::{Hash, Hasher};
use std::borrow;

/// An allocation with a lifetime managed by Regen instead of by the Rust compiler.
/// From Rust's perspective it looks like a permanent allocation which might be
/// mutated, and so it is not safe.
#[derive(Clone)]
#[repr(C)]
pub struct Ptr<T> {
  pub p : *mut T
}

impl <T> Ptr<T> {
  pub fn to_ptr(v : Self) -> *mut T {
    v.p
  }

  pub fn to_u64(v : Self) -> u64 {
    v.p as u64
  }

  pub fn to_i64(v : Self) -> i64 {
    v.p as i64
  }

  pub unsafe fn from_ptr(p : *mut T) -> Self {
    // TODO: mark as unsafe
    Ptr { p }
  }

  pub unsafe fn from_u64(v : u64) -> Self {
    // TODO: mark as unsafe
    Ptr { p: v as *mut T }
  }
}

impl <T : Clone> Copy for Ptr<T> { }

/// A pointer to a Regen slice (See the documentation for Ptr)
#[derive(Clone)]
#[repr(C)]
pub struct SlicePtr<T> {
  pub len : usize,
  pub p : *const T,
}

impl <T : Clone> Copy for SlicePtr<T> { }

pub fn alloc<T>(t : T) -> Ptr<T> {
  Ptr { p : Box::into_raw(Box::new(t)) }
}

pub fn alloc_slice<T, C>(values : C) -> SlicePtr<T>
  where C : Into<Vec<T>>
{
  let vs : Vec<T> = values.into();
  let p = SlicePtr { p : vs.as_ptr(), len: vs.len() };
  std::mem::forget(vs);
  p
}

impl<T> Deref for Ptr<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    unsafe { &*self.p }
  }
}

impl<T> DerefMut for Ptr<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    unsafe { &mut *self.p }
  }
}

impl <T : PartialEq> PartialEq for Ptr<T> {
  fn eq(&self, other: &Self) -> bool {
    (&**self) == (&**other)
  }
}
impl <T : Eq + PartialEq> Eq for Ptr<T> {}

impl<T: Hash> Hash for Ptr<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    (**self).hash(state);
  }
}

impl<T: fmt::Display> fmt::Display for Ptr<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&**self, f)
  }
}

impl<T: fmt::Debug> fmt::Debug for Ptr<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(&**self, f)
  }
}

impl<T> borrow::Borrow<T> for Ptr<T> {
  fn borrow(&self) -> &T {
    &**self
  }
}

impl<T> AsRef<T> for Ptr<T> {
  fn as_ref(&self) -> &T {
    &**self
  }
}

impl <T: 'static> SlicePtr<T> {
  pub fn as_slice(&self) -> &'static [T] {
    unsafe { std::slice::from_raw_parts(self.p, self.len) }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn slice_range(&self, r : Range<usize>) -> SlicePtr<T> {
    let s = &self.as_slice()[r];
    SlicePtr{ p: s.as_ptr(), len: s.len() }
  }
}

impl <T : PartialEq + 'static> PartialEq for SlicePtr<T> {
  fn eq(&self, other: &Self) -> bool {
    self.as_slice() == other.as_slice()
  }
}
impl <T : Eq + PartialEq + 'static> Eq for SlicePtr<T> {}

impl<T: Hash + 'static> Hash for SlicePtr<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_slice().hash(state);
  }
}

impl<T: fmt::Debug + 'static> fmt::Debug for SlicePtr<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self.as_slice(), f)
  }
}

impl <T: 'static> Index<usize> for SlicePtr<T> {
  type Output = T;

  fn index(&self, i: usize) -> &Self::Output {
    &self.as_slice()[i]
  }
}

impl <T: 'static> Index<RangeFrom<usize>> for SlicePtr<T> {
  type Output = [T];

  fn index(&self, r: RangeFrom<usize>) -> &Self::Output {
    &self.as_slice()[r]
  }
}

impl<T : 'static> IntoIterator for SlicePtr<T> {
  type Item = &'static T;
  type IntoIter = std::slice::Iter<'static, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.as_slice().iter()
  }
}

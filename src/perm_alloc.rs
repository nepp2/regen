/// Some allocation facilities to simplify borrow checking by relying
/// on the fact that these allocations will live until the process
/// terminates.
/// In future this may be adapted into a region-allocation system, in which
/// regions can be dropped. This will be unsafe from Rust's perspective, but
/// will instead reflect the semantics of the Regen language.

use std::fmt;
use std::ops::{Deref, Index};
use core::hash::{Hash, Hasher};
use std::borrow;
use std::ops::RangeFrom;

#[derive(Copy, Clone)]
pub struct Perm<T> {
  p : *const T
}

#[derive(Copy, Clone)]
pub struct PermSlice<T> {
  p : *const T,
  len : usize,
}

pub fn perm<T>(t : T) -> Perm<T> {
  Perm { p : Box::into_raw(Box::new(t)) }
}

pub fn perm_slice<T : Clone>(vs : &[T]) -> PermSlice<T> {
  if vs.len() == 0 {
    PermSlice { p: 0 as *const T, len: 0 }
  }
  else {
    let v : Vec<_> = vs.iter().cloned().collect();
    let p = PermSlice { p : v.as_ptr(), len: vs.len() };
    std::mem::forget(v);
    p
  }
}

impl<T> Deref for Perm<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    unsafe { &*self.p }
  }
}

impl <T : PartialEq> PartialEq for Perm<T> {
  fn eq(&self, other: &Self) -> bool {
      (&**self) == (&**other)
  }
}
impl <T : Eq + PartialEq> Eq for Perm<T> {}

impl<T: Hash> Hash for Perm<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: fmt::Display> fmt::Display for Perm<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Perm<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T> borrow::Borrow<T> for Perm<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T> AsRef<T> for Perm<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}


impl <T: 'static> PermSlice<T> {
  pub fn as_slice(&self) -> &'static [T] {
    unsafe { std::slice::from_raw_parts(self.p, self.len) }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn slice_range(&self, r : RangeFrom<usize>) -> PermSlice<T> {
    let s = &self.as_slice()[r];
    PermSlice{ p: s.as_ptr(), len: s.len() }
  }
}

impl <T : PartialEq + 'static> PartialEq for PermSlice<T> {
  fn eq(&self, other: &Self) -> bool {
      self.as_slice() == other.as_slice()
  }
}
impl <T : Eq + PartialEq + 'static> Eq for PermSlice<T> {}

impl<T: Hash + 'static> Hash for PermSlice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl<T: fmt::Debug + 'static> fmt::Debug for PermSlice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

impl <T: 'static> Index<usize> for PermSlice<T> {
  type Output = T;

  fn index(&self, i: usize) -> &Self::Output {
    &self.as_slice()[i]
  }
}

impl <T: 'static> Index<RangeFrom<usize>> for PermSlice<T> {
  type Output = [T];

  fn index(&self, r: RangeFrom<usize>) -> &Self::Output {
    &self.as_slice()[r]
  }
}

// impl<'a, T> Iterator for &'a PermSlice<T> {
//   type Item = &'a T;

//   fn iter(self) -> Self {
//       self.as_slice().iter()
//   }
// }


impl<T : 'static> IntoIterator for PermSlice<T> {
  type Item = &'static T;
  type IntoIter = std::slice::Iter<'static, T>;

  fn into_iter(self) -> Self::IntoIter {
      self.as_slice().iter()
  }
}

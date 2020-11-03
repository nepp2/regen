
#[derive(Copy, Clone)]
pub struct RegenString {
  pub ptr : *const u8,
  pub len : u64,
}

impl RegenString {
  pub fn as_str(&self) -> &str {
    unsafe { 
      let s = std::slice::from_raw_parts(self.ptr, self.len as usize);
      std::str::from_utf8_unchecked(s)
    }
  }
}

pub fn from_string(s : String) -> RegenString {
  let ptr = s.as_ptr();
  let len = s.len() as u64;
  std::mem::forget(s);
  RegenString { ptr, len }
}

use crate::perm_alloc::{Ptr, perm};


#[derive(Clone, Copy)]
#[repr(C)]
pub struct Resource {
  pub resource : *mut (),
  pub destructor : fn(*mut ()),
}

#[derive(Clone)]
pub struct Region {
  resources : Vec<Resource>,
}

pub fn region_alloc(r : Ptr<Region>, bytes : u64) -> *mut () {
  let TODO = (); // this still allocates with the global allocator
  let layout = std::alloc::Layout::from_size_align(bytes as usize, 8).unwrap();
  unsafe { std::alloc::alloc(layout) as *mut () }
}

pub fn create_region() -> Ptr<Region> {
  let r = Region { resources: vec![] };
  perm(r)
}

pub fn free_region(region : Ptr<Region>) {
  for r in &region.resources {
    (r.destructor)(r.resource)
  }
}


use crate::perm_alloc::{Perm, perm};


#[derive(Clone, Copy)]
struct Resource {
  resource : *mut (),
  destructor : fn(*mut ()),
}

struct Region {
  resources : Vec<Resource>,
}

fn create_region() -> Perm<Region> {
  let r = Region { resources: vec![] };
  perm(r)
}

fn free_region(region : Region) {
  for r in region.resources {
    (r.destructor)(r.resource)
  }
}


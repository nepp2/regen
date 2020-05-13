

#[derive(Copy, Clone)]
enum TypeTag {
  Byte,
  Tuple,
  Array,
}

union TypeUnion {
    tuple: Tuple,
    name: Name,
    array: Array,
  }
  
#[derive(Copy, Clone)]
struct Type {
  tag : TypeTag,
  content : *mut TypeUnion,
}

#[derive(Copy, Clone)]
struct Tuple {
  member_count : u64,
  first_member : *mut Type,
}

#[derive(Copy, Clone)]
struct Name {
  symbol : u64,
  type_value : Type,
}

#[derive(Copy, Clone)]
struct Array {
  length : u64,
  type_value : Type,
}

struct Def {
  symbol : u64,
  type_val : Type,
}

struct Environment {
  symbol_table : Vec<String>,
  defs : Vec<Def>,
}


# Bootstrapping

Minimal language for self-extension. Low-level, like C. No memory model built-in.

## Plan

- S-expression bytecode interpreter initially
- Support ffi calls into C code
- Use this to implement optional LLVM JIT support.

## Requirements

- functions, globals and types are all just values stored in "defs"
- eval
- only one control flow mechanism
- types are values?
- all functions/values/types use same type of "def"
- values are unboxed with varying sizes

# Unpicking types, structs and macros

I want to find a starting point here for bootstrapping in the simplest way possible. The problem is that they all depend on each other to some extent.

- Structs
  - struct definition syntax requires a macro
  - structs require some notion of "type"

- Types
  - the "type" type needs to be a struct

- Macros
  - these need to process the AST, which may require structs

## Possibilities

Define the type struct clumsily using functions



Type:
 - int
 - id
 - tuple

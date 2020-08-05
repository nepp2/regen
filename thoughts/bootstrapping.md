
# Bootstrapping

Minimal language for self-extension. Low-level, like C. No memory model built-in.

## Plan

- S-expression bytecode interpreter initially
- Support ffi calls into C code
- Use this to implement optional LLVM JIT support.

## Requirements

- intrinsics
- eval
- only one control flow mechanism
- types are values?
- all functions/values/types use same type of "def"
- values are unboxed with varying sizes

Type:
 - int
 - id
 - tuple

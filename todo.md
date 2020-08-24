
# Immediate TODO

- struct support
  - stack allocate arbitrary sized blocks
  - return pointer to start of block
  - set fields crudely using pointer maths
  - make it pretty using macros

- figure out how to unpick the bootstrapping of structs, macros and types

- make it possible to store a struct in a register (and return one from a function)
  - requires knowing the type of a def?
  - means functions must have known return type?

# Future

- eval function?
- struct support?
  - layout based on pointer alignment
- macro support?
- pointer dereferencing?

# Questions

## Macros and functions

What is the core form of a macro? When it is resolved by the compiler, its type indicates that it is a macro, and it is immediately queued for evaluation.

What if every expression is macro evaluated aggressively?

- the `fun` command is really a macro?
- it defines a function at compile-time, and evaluates into a function pointer
- so there should really be a `new_function` intrinsic, which behaves like a function?
- this can then be used to implement a `fun` macro

## Globals

Globals are resolved in the interpreter. Should they be resolved during codegen?

I think this should be safe, as long as the address of the global remains stable when it is updated. This is no problem as long as the value can't exceed 64 bits. Anything that does is stored as a pointer.

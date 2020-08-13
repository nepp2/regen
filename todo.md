
# Immediate TODO

- Handle semicolon comments in parser
- Define milestone 2
- Implement milestone 2

# Future

- Figure out vertical slice
  - As a sequence of milestones 
- eval function
- struct support
  - layout based on pointer alignment
- macro support
- pointer dereferencing

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

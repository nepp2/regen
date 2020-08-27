
# Immediate TODO

## Macros

The next task is to make a macro expander.

Any function call tagged as a macro is expanded immediately.

Is this a separate pass, or does it just happen during the codegen pass?

- Have an alternative symbol table
- Store macros with a flag
  - Every macro has the same type, so this might work.
  - The type is `fn(&[Node]) -> Node`


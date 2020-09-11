
# Immediate TODO

Get macros working
- Draft the code to define macros
- Add a macro flag type
  - Nests a function type
- Write the manual code needed to define the macro syntax
  - this includes running eval
  - do i need to retrieve a value from eval? maybe not

## Macros

The next task is to make a macro expander.

Any function call tagged as a macro is expanded immediately.

### Template syntax

```lisp
  (template
    (def #name (fun #args #body))
  )
```


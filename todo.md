# TODO

- [x] new reactive def system
  - [x] support observe keyword
    - [x] compile observe expressions into CellUid pointers with poly "signal" types
    - [x] register observer dependencies in env
    - [x] check reactive defs for signal updates _after_ checking them against the normal graph
  - [x] making polling work properly
    - [x] don't compute defs unless the dependencies have values
  - [ ] make compiler fail gracefully
    - [ ] fail silently when a dependency is found, but is not ready for use
    - [ ] leave a marker for cells whose values couldn't be calculated
      - [ ] just log the expression and the missing dependency
  - [x] make namespacing global, regardless of context
    - [x] fix the list implementation
- [x] magic polymorphic container and stream functions



the first time an expression is compiled, its dependencies aren't known. when compiling, it may try to use a value that hasn't been initialised. it should fail and wait, instead of throwing an error. initially it's probably fine if it just throws and error though?



`observe uid` is maybe a bug? it creates a runtime value that can be stored (for example, in a reactive container). it's probably fine as long as the runtime checks that the uid is valid whenever it binds it to an observer.
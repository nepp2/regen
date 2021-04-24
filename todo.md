# TODO

- [ ] new reactive def system
  - [x] support observe keyword
    - [x] compile observe expressions into CellUid pointers with poly "signal" types
    - [x] register observer dependencies in env
    - [x] check reactive defs for signal updates _after_ checking them against the normal graph
  - [ ] magic polymorphic container and stream functions



the timer doesn't update the cell

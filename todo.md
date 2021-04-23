# TODO

- [x] fix syntax highlighting (missing keywords, etc)
- [x] fix string highlighting
- [x] add signal types
  - [x] add nominal type C function
  - [x] signal as nominal type
  - [x] support poly types
  - [x] cast signal to poly signal
- [x] parse new syntax
  - [x] reactive
  - [x] onchange
  - [x] container
  - [x] stream
- [ ] trigger hot reloading from reactive cell updates
- [ ] compile container and stream
- [ ] support onchange



problems:

* existing hotloading system needs to cooperate with the new one
* should store defs even if they are broken



namespaces and metaprogramming make everything very complicated. the value is that we get things like lists.

the embed problem is currently just an edge-case.
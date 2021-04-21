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
- [ ] compile container and stream
- [ ] support onchange
- [ ] trigger hot reloading from container updates



thoughts:

* "reactive" just expects a typed signal

* container and stream create signals
* I want to get rid of observers, because they don't obey the graph ordering
* what does container do?
* what does stream do?


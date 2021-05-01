# TODO

* [x] fix observer triggers
* [x] make simple drawing demo work again
* [x] optimise CellUid
* [ ] optimise compilation
  * [ ] divide compilation and evaluation into two passes
  * only recompile if a code dependency changed
  * otherwise just re-execute previous function
* [ ] optimise reactive events

  * if there are no embeds, record the update sequence
* [x] make compiler fail gracefully

* [ ] fix observe keyword
  * keeping a uid reference as a value can lead to problems
  * Short-term solution: the `observe` keyword can only be used as the first argument of a `stream` or `container` call
* [ ] Support `merge` keyword
  * got to cope with not being able to assume a single reactive source

# Issues


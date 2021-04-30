# TODO

* [x] fix observer triggers
* [ ] make simple drawing demo work again
* [ ] optimise CellUid
  * introduce CellIdentifier
  * turn them into CellUids (simple numbers)
  * destroy old CellUids when `unload_cell` is called
  * is this safe?
* [ ] optimise compilation
  * [ ] divide compilation and evaluation into two passes
  * only recompile if a code dependency changed
  * otherwise just re-execute previous function
* [ ] optimise reactive events

  * if there are no embeds, record the update sequence
* [ ] make compiler fail gracefully
  
  * [ ] fail silently when a dependency is found, but is not ready for use
  * [ ] only display an error once until something changes (what?)
* [ ] fix observe keyword
  * keeping a uid reference as a value can lead to problems
  * Short-term solution: the `observe` keyword can only be used as the first argument of a `stream` or `container` call
* [ ] Support `merge` keyword
  * got to cope with not being able to assume a single reactive source

# Issues


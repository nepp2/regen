# TODO

* [ ] fix observer triggers
  * trigger when the reactive dependency changes
  * mark as not initialised if a value/code dependency changes?
  * compiles on-demand
  * what if value references another def that has been deallocated?
    * require user copying?
* [ ] optimise compilation
  * only recompile if a code dependency changed
  * otherwise just re-execute previous function
* [ ] optimise reactive events
  * if there are no embeds, record the update sequence
* [ ] make compiler fail gracefully
  * [ ] fail silently when a dependency is found, but is not ready for use
  * [ ] only display an error once until something changes (what?)

# Issues

### `observe` keyword

Keeping a uid reference as a value can lead to problems.

Short-term solution: the `observe` keyword can only be used as the first argument of a `stream` or `container` call


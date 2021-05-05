# TODO

* [x] fix reactive def linking issue
* [ ] solve issue with sample cells
  * value changes shouldn't trigger stream cells
  * what about container cells?
  * do I need sample cells, as a concept?
* [ ] optimise reactive events

  * if there are no embeds, record the update sequence
* [ ] fix observe keyword
  * keeping a uid reference as a value can lead to problems
  * Short-term solution: the `observe` keyword can only be used as the first argument of a `stream` or `container` call
* [ ] Support `merge` keyword
  * got to cope with not being able to assume a single reactive source

# Issues


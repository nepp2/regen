# TODO

* [ ] optimise reactive events
  * [x] get the topological ordering
  * [ ] turn active cells list into a HotloadState field
* [ ] swap to traditional syntax
  * [ ] function definition
  * [ ] struct definition
* [ ] improve scrub control
* [ ] fix observe keyword
  * uid references should not be treated as values
  * possible solution:
    * the `observe` keyword can only be used as the first argument of a `stream` or `container` expression
    * `stream` and `container` calls can only be used as part of a reactive def expression
* [ ] Support `merge` cells
  * [ ] got to cope with not being able to assume a single reactive source
* [ ] Support `sample` cells

# Issues


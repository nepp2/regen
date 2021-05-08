# TODO

* [ ] swap to traditional syntax
  * [ ] function definition
  * [ ] struct definition
* [ ] improve event timer control
* [ ] optimise reactive events
* if there are no embeds, record the update sequence
* [ ] fix observe keyword
  * uid references should not be treated as values
  * possible solution:
    * the `observe` keyword can only be used as the first argument of a `stream` or `container` expression
    * `stream` and `container` calls can only be used as part of a reactive def expression
* [ ] Support `merge` cells
  * [ ] got to cope with not being able to assume a single reactive source
* [ ] Support `sample` cells

# Issues


# TODO

* [ ] prevent observe keyword misuse
  * uid references should not be treated as values
  * simplest solution:
    * the `observe` keyword can only be used as the first argument of a `stream` or `container` expression
    * `stream` and `container` calls can only be used as part of a reactive def expression
* [ ] Support `merge` cells
  * [ ] got to cope with not being able to assume a single reactive source
* [ ] Support `sample` cells

# Issues


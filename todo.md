# TODO

* [x] **Fix embed**

  * [x] Embed does not currently work properly, because it is being evaluated in the context of the containing expression, which is not linked against its dependencies.
  * [x] Easiest fix is probably to say that embed can't reference the local scope, and so it is not compiled into the containing expression. Embeds are const_exprs and resolve to constant values.

* [ ] support reactive includes?

  * Reactive definitions don't work in external modules

  * Support primitive event_loop watcher events (similar to timers)
  * [x] **Try using embed to include files**

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


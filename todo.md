# TODO

* [ ] make compiler fail gracefully
  * [ ] fail silently when a dependency is found, but is not ready for use
  * [ ] leave a marker for cells whose values couldn't be calculated
  * [ ] just log the expression and the missing dependency

`observe uid` is maybe a bug? it creates a runtime value that can be stored (for example, in a reactive container). it's probably fine as long as the runtime checks that the uid is valid whenever it binds it to an observer.

avoiding repetitive errors is hard. the problem is that if something fails to compile, i have to re-execute it _every time_ any event is triggered, because i don't know what it depends on. this is very unfortunate. it could be solved by only allowing each cell to display one error between successful compiles, but there will still be repetitive compile attempts happening behind the scenes.

alternatively, i could store the dependencies of an expression before compiling it. i'm not sure why i stopped doing this. some problem with embed?

then there's four cell states:

* expr located
  * only update if expr changed
* dependencies extracted
  * only update if a dependency changed
* function compiled
* value calculated


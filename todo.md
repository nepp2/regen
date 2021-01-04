
# Immediate TODO

* Fix the code/value dependency issue
  * `(def val 5)`
  * `(def f (fun () (val)))`
  * change `val` def: `(def val 6)`
  * causes `f` to be recompiled

# Issues

* Compiler panics in response to syntax/type errors
  * Currently I catch panics
  * I should use proper error handling for a better and more reliable experience

* Replace event polling loops with event streams

* Free memory of old/deleted nodes
  * Note that SrcLocation structs reference the full source for a file
    * Replace all expr nodes and delete old source strings

* `include` doesn't work properly with hotloader
  * if an include statement is hotloaded, it will clumsily try to overwrite previous definitions
  * included definitions won't hotload if their source file is changed
  * should be implemented via the event system
    * create a file watch stream
    * hook it into a graph node
    * provide a way to insert & evaluate a list of definitions in the top level

# Roadmap

## Implementing event streams

Can't really do live rendering/game examples without either hacking a limited example together (like a processing-style environment), or without building in support for event streams.

I don't know how the event streams are supposed to work yet. This is really the next big problem to solve. The plan was to base them on the Observable design, because that seems fairly simple.

There has to be a central event loop, and it needs to find the event streams when some code is loaded. It needs to be able to push events to functions that are observing them.

Plan:

* Have an event type (struct of pointers to functions/state/whatever)
  * source state (e.g. sdl reference)
  * poll function
* Have a built-in function for creating event streams
  * it will be found on the env
  * in future it can change as the local context changes
* Have an observe function for registering listeners

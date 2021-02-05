# Kanban issue

* Figure out what comes after basic stream operations

## Immediate TODO


# Issues

* Add support for binding event streams to defs

* Fix event handler ordering

* Fix the code/value dependency issue
  * `(def val 5)`
  * `(def f (fun () (val)))`
  * change `val` def: `(def val 6)`
  * causes `f` to be recompiled
  * Is this actually a blocker?
    * It is incredibly slow and stupid behaviour, but not actually wrong

* Compiler panics in response to syntax/type errors
  * Currently I catch panics
  * I should use proper error handling for a better and more reliable experience

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

## Streams 2.0

contemplating pull-based streams instead of push-based



# Kanban issue

* Support signal defs

# Roadmap

## Demo

* is hotloading external modules important? NO
* signal into def - why?
* def into signal - why?
* nodes for storing state - why?

## New features

* turn a signal into a def
* turn a def into a signal (user input - less important?)
* node keyword (holds state)
  * maybe it should be "state" or "container" or something?
* embed an expr value into a graph
* graph literals for embedded graphs
  * graphs are evaluated once at the point of creation
  * their defs are not global by default
  * their defs can be accessed via field index syntax
  * they don't need different types, because the value is always available at compile-time
* open keyword for importing symbols from embedded graph

* **TODO:** some form of generic support!

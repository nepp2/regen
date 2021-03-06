# Kanban issue

* Figure out how to compile/evaluate graph templates

# Roadmap

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

### Overloads

I have syntax like this:

```
  def add::[i64] = fun(a : i64, b : i64) {
    a + b
  }
```

However, the def name must be inferred from callsites like `add(a, b)`.

This means that the overloaded function must be indexed by type.

Templated defs are not indexed by type, they are indexed by cell UID.

This works as long as every type maps to the same cell UID, but that is not the case. Types are just values. You can create 50 of them in a single cell initialiser.

We need a mapping from type to cell UID. One possibility is:

`type -> const expr -> CellUID`

simplest solution: overloading only works with nominal types, and nominal types use cell UIDs.
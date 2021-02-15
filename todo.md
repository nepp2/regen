# Kanban issue

* Figure out what new features i need

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

notes:
  * stream renamed to signal (why?)

### Templates

Templated definitions are functions which run once, and then cache their output.

*Question: isn't this just the same as a def binding? is there any reason for templated defs other than neater function calls?*

What is required?
  * pass type values to defs
  * pass string values to include?
  * infer type arguments from call-sites

A template parameter can be any literal value or any global def reference. The point is just that it must have a single, consistent value everywhere it appears in the program.

Graph literals complicate the use of defs because they have a different namespace. It might be sensible to make graph literal defs a part of the global namespace too, but prefixed with some compiler-generated id. This could make name resolution harder.

Alternatively a graph literal, when invoking a template, could check where its own defs are sourced from.

### Template alternative

Use the terra approach of manually templating and compiling values.

Downside? It's hard to write functions for templated types. You have to instantiate those as well. Although, they can be instatiated at the same time as the type, sometimes.

Consider including a template-style name syntax to paper over this issue. e.g. a syntax where something like `list[i64]` is actually just a symbol. This would mean finding an alternative array index syntax.

Function calls would be a bit clumsy:

`list.add[i64](v);`

Alternate array syntax:
  * `ns.[i]`, like F#
  * `ns(i)`, like Scala
  * ``


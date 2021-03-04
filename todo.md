# Kanban issue

* Support template-style overloading
* Support template generators

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

### Templates

Templated definitions are functions which run once, and then cache their output.

**Question: isn't this just the same as a def binding? is there any reason for templated defs other than neater function calls?**
  * Caching the value returned is important, but const expressions handle this
  * 

What is required?
  * pass type values to defs
  * pass string values to include?
  * infer type arguments from call-sites

A template parameter can be any literal value or any global def reference. The point is just that it must have a single, consistent value everywhere it appears in the program.

Graph literals complicate the use of defs because they have a different namespace. It might be sensible to make graph literal defs a part of the global namespace too, but prefixed with some compiler-generated id. This could make name resolution harder.

Alternatively a graph literal, when invoking a template, could check where its own defs are sourced from.

# Template defs

Handle overloading first!

templates can be indexed with certain key-compatible values
templates store a generator in a single def
they cache their instances in named defs, but these can't be directly defined.
They only do this to support subdefs, which I am now stuck with.

consider `eq` function:
* generic
* generator doesn't depend on much
* specialisations all depend on generator
* each specialisation also depends on types/values referenced

how can eq be overridden for specific types?

what if templates can only accept def names? no const expressions?
or maybe it's okay if it's just cell ids?

where is the generator template stored? need to differentiate by number of arguments?

# Big question:

Is it okay for cell dependencies to be discovered only during type checking?

The most obvious reason it's _not_ okay is that it becomes impossible (or just hard?) to infer the correct global ordering before compiling cells.

The way around this issue is to compile cells in their order of appearance by default, and require each cell to appear before everything that depends on it.

Does this break hotloading? I think it doesn't as long as this ordering is _only_ used to guarantee dependency ordering. There must be no hidden interactions between cells.

What if I build some UI that doesn't provide a global cell ordering?

The ordering could instead be calculated using some multi-pass system, as long as cell identities are never actually ambiguous. First pass finds all the names, second pass resolves them. The only problems are caused by macros (e.g. the `embed` command), and that was already true.



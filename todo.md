
# Slice problem

* I don't have support for array slices yet, which is a problem because array types aren't compatible with other arrays of different length
* I can't support them as user types because there are no generics yet
* I can hack them in as special types, but then I'm really adding complexity

# Types, Macros and staged metaprogramming

I have a problem separating out some compilation passes.

* Need to find the dependency graph before attempting to typecheck
  * this means spotting all the def reference nodes
  * otherwise global def types won't be available

How could it work without macros?

* Find structure (and therefore the def nodes)
* Find types and codegen

Simple, fast solution:

* Remove macros from the language
  * Are they important?
  * Probably, but not yet.
* Turn a few key macros into compiler built-ins (e.g. loops)
* Expand the built-in macros during the structure parse phase

Should I keep this structure parsing stuff?
  * Can separate type checking from codegen, but do I need to?
  * Not yet. But now I have to throw away code. Will I need that code?
  * I still need to do a lot of structure parsing just to expand macros and find the def references, so maybe I should just keep it...
  * It doesn't mean that I need to split up the type checking


# Hotloading TODO

## Critical

* Build symbol dependency graph
  * Find all of the symbols referenced in an expression!
  * Can't spot globals without parsing properly
* Update all nodes affected by a change
* Remove deleted nodes

## Secondary

* Prevent compiler from panicking (it should fail gracefully)
  * Just run it in a thread? (i think panicking only kills threads)
* Replace event polling loops with event streams
* Free memory of old/deleted nodes
  * Note that SrcLocation structs reference the full source for a file
    * Replace all expr nodes and delete old source strings


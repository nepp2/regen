
# Bootstrapping - second pass

I've been trying to bootstrap the whole language from _extremely_ basic core elements. For example, I'm trying to bootstrap tuples using macros, and I have barely any typechecking. This has turned into a bit of a mess.

I could:

* Improve basic type checking
* Include tuples in the core language

This would mean that structs still require macros.

Using macros for such basic language elements seems like it could cause a lot of slowdown in the compiler. The execution of the macros could be sped up in the future, but they would still be generating lots of extra AST nodes everywhere.

On the other hand, stable macros can be moved into the compiler later. The important bit is being able to experiment easily. At the moment I cannot do that.

## Core language and types

Core types:
* Primitives
* Functions
* Macros
* Tuples

Every single type check is like a function application?

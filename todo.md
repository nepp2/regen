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


# Templates & Overloading

The problem is that I currently need to know the dependencies _before_ type checking, because the types themselves are dependencies.

I could instead enforce line-number ordering as an additional dependency constraint, and then discover dependencies during the type-check phase. I'm wary of doing this because it will be hard to reverse, and I don't know what other unwanted implications it has.

The only other option I can see is to say that overloads are a special type of function, and all calls depend on that function. This dependency is used to order compilation, but could be narrowed after-the-fact. In this scenario, the overload function is evaluated during type-checking, and returns the function that will actually be called.

# Graph templates

Graph templates could solve a lot of the problems I have. A graph template is instantiated using const-expr arguments, and can then be cached.

The output of a graph template is a single value.

I need to implement simple subgraphs first.

* [x] Parse subgraph into expr
* [x] Semantic pass needs to treat each sub-expression as a separate function

# Subgraph design

Any def can contain defs, and is thus a subgraph.

Templates are then just a def with parameters.

All cells in a subgraph are tracked in the same global environment. It's just a question of visibility. cells defined inside a def should not be visible outside it, without mentioning their namespace.

```regen
  def a = 10;
  def b = 10;
  def c = 10;

  def foo = {
    def b = 20;

    def bar {
      def c = 30;
    }
  }
```

globally, this defines:

* `a`
* `b`
* `c`
* `foo::b`
* `foo::bar::c`

Within `foo::bar` we have to find symbols from all three namepsaces. Priority must be given to the most local scope.

Def can be a (namespace, symbol) pair. A namespace can be a PtrSlice of symbols.

## Shadowing problem

Two constant expressions defined in different namespaces may not be equivalent.

In fact it's very difficult to tell if a cell has been changed by the introduction of a more local def, which shadows an existing def.

```regen
  def a = 1;

  def foo = {
    def a = 2; // this line is hotloaded in after the initial evaluation

    a
  }
```

The nested `a` could find the def it is shadowing, and mark its dependents to indicate that they should check their dependencies again.

Shadowed defs could be stored on some kind of def stack, and every def encountered in within the namespace checks its dependents against everything in the def stack.

I should seek a solution to this shadowing problem that is compatible with an evaluation mode that doesn't depend on def order.

## Namespaced const expression

```regen
  def foo = {
    def a = i64;
    fun (v : a) {
      v + v
    }
  }

  def a = i32;

  fun bar(v : a) {
    v + v
  }
```
The problem here is that the value of the constant expression `a` will be cached as `i64`, which will be used for both `foo` and `bar`. In reality it is sensitive to the def scope.

So const expressions should also be namespaced? I suppose they can be copied from each other if their dependencies are equal.

# Rewrite hotload

* Iterate over loaded cells
  * Equality comparison on exprs of all loaded cells
    * Mark changed as appropriate
    * Add to eval list
  * Keep track of all new cells
* Iterate over module's cells
  * Overwrite exprs (to update location data)
  * Mark the deleted cells (they didn't appear in the new code)
  * Question: should cells be grouped by module in the environment?
* Iterate over changed cells
  * Mark their dependents as changed, recursively
* 

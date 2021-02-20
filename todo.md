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

## Problem

I use def names or expr equivalence to identify a def. How do I identify a graph? For example:

```
def v = {
  let a = graph {
    def x = 10;
    def y = x + 20;
  };
  let b = graph {
    def x = 20;
    def y = x + 30;
  };
  a + b
};
```

Suppose the first `x` value is changed. How do we know which `x` to update? How do we know which `y` to update? The problem is that we don't actually know which graph we are in. We can't just use the structure of the graph, because it has changed. It won't match either of the original graphs.

The simplest solution is to give graphs mandatory names, like defs:

```
def v = {
  graph a {
    def x = 10;
    def y = x + 20;
  };
  graph b {
    def x = 20;
    def y = x + 30;
  };
  a + b
};
```

Then graphs and defs can appear anywhere, but they are obscured behind the parent def. In that case, the graph keyword is not really achieving anything. This could just as easily be written as:

```
def v = {
  def a = {
    def x = 10;
    def y = x + 20;
  };
  def b = {
    def x = 20;
    def y = x + 30;
  };
  a + b
};
```


# Kanban issue

* Add the missing event stream functions


## Immediate TODO

* Answer the stream merging question

# Issues

* Add support for binding event streams to defs

* Fix event handler ordering

* Rename defs to "cells"?

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

* Integrate event streams

## Implementing event streams

Some issues:

* the design of sample stream implies that streams must _always_ contain a default value
* the design of merge stream is ambiguous, particulary when both input streams share a root stream

these aren't really event streams. they are state containers.

the only real operation they support is:

```rust
  // returns true if the state actually changed
  fn update(mut state : State) -> bool
```

or they are streams because they push updates regardless of whether the value changed?

or a change can be forced using a counter (for example)

a signal might be a better term, but i'm not sure

it's unclear how to create the desired merge behaviour
  * error if both inputs are triggered at the same time?
  * how to detect which is triggered?

should triggers be different to streams?

do the semantics of the event graph even matter? do i care if it's possible to make a stupid graph, as long as it's also possible to make a good graph? should i be following the observable pattern of JSX?

## Syntax

```scala
  def tick = timer(60);

  def input = {
    val s = sdl_input_stream();
    val input = map_stream(s, input_to_game_event);
    val time = map_stream(tick, time_to_game_event);
    merge_stream(input, time)
  };

  def model = state_stream(tick, 0, (state, tick_value) => {
    state += tick_value;
  });

  def view = state_stream(tick, renderer, (r, tick_value) => {
    val m = sample(model);
    render(m)
  });
```

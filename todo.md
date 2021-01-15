# Immediate TODO

* Integrate event streams

* Fix the code/value dependency issue
  * `(def val 5)`
  * `(def f (fun () (val)))`
  * change `val` def: `(def val 6)`
  * causes `f` to be recompiled
  * Is this actually a blocker?
    * It is incredibly slow and stupid behaviour, but not actually wrong

# Issues

* Compiler panics in response to syntax/type errors
  * Currently I catch panics
  * I should use proper error handling for a better and more reliable experience

* Replace event polling loops with event streams

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

## Implementing event streams

I created a stream-based SDL example in `scratchpad.gen`.

It requires some functions for creating event streams:
  * `timer_stream`
    * args:
      * millisecond_interval: `i64`
    * returns: `stream<tick_event>`
  * `filter_stream`
    * args:
      * input_stream: `stream<event>`
      * initial_state: `state`
      * poll_function: `fun(*state, *event) -> bool`
    * returns: `stream<state>`
  * `merge_stream`
    * args:
      * a: `stream<A>`
      * b: `stream<B>`
      * merge_a: `fun(*A) -> C`
      * merge_b: `fun(*B) -> C`
    * returns: `stream<C>`
  * `state_stream`
    * args:
      * input: `stream<event>`
      * initial_state: `state`
      * update: `fun(*state, *event)`
    * returns: `stream<state>`
  * `sample_stream`
    * args
      * source: `stream<E>`
      * sampler: `stream<T>`
    * returns: `stream<(E, T)>`

The trouble with these functions is that most of them are polymorphic, and they even refer to polymorphic types. I could iron over this polymorphism by using either void pointers or some kind of limited type-erasure mechanism.

Alternatively, I could try to find the simplest path to some kind templating system, likely based on Terra. Ultimately if the event loop is implemented in Rust, the FFI will have to be monomorphic. LibUV is a very successful high-performance event loop library, and it is written in C so it must also be monomorphic.

I haven't considered how an event stream gets hooked into the core event loop. Should every event stream be immediately hooked in upon creation? This is okay as long as they are destroyed along with the `def` that introduced them.

## Implementing streams 2

I can bind streams to state cells, but something has to push events through the stream.

All events are pushed by other streams

The root stream type is a timer

Event streams are usually implemented as a filtered map or a fold. For example:

  * a stream of packets would be a filtered map on a timer (although it holds state?)
  * a state stream would be a fold

What if a stream needs to hold state but not push it? arguably the packet stream is doing this, because the socket is modelled as state.

This is perhaps the strength of observers. They can have any internal state, unrelated to the type of event that they push.

So a stream type would be like:

```rust
struct Stream<Input, State, Output> {
  state : State,
  handle_event : fn (state : &mut State, event : Input) -> bool,
}
```

If you want to push a bunch of events, you have to output a vec (or a slice). It's up to the receiver to flatten them. The stream only pushes to dependent streams if `handle_event` returns true.

As a monomorphic struct, it looks like this:

```rust
struct Stream {
  state : *mut (),
  handle_event : fn (s : *mut (), event : *const ()) -> bool,
}
```

This API should eventually handle failures. Possibly as follows:

```rust
struct Stream {
  state : *mut (),
  handle_event : fn (s : *mut (), event : *const ())
    -> Result<bool, HandlerError>,
}
```

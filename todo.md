
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
  * `timer_framerate`
    * args:
      * framerate: `u64`
    * returns: `stream<tick_event>`
  * `source_stream`
    * args:
      * poll_function: `fun(*void, *E, *tick_event) -> bool`
      * source: `*void`
      * initial_state: `E`
      * sampler: `stream<tick_event>`
    * returns: `stream<E>`
  * `merge_stream`
    * args:
      * a: `stream<A>`
      * b: `stream<B>`
    * returns: `stream<C>`
  * `state_stream`
    * args:
      * initial_state: `S`
      * update: `fun(*E, *S)`
      * input: `stream<E>`
    * returns: `stream<S>`
  * `sample_stream`
    * args
      * source: `stream<E>`
      * sampler: `stream<T>`
    * returns: `stream<E>`

The trouble with these functions is that most of them are polymorphic, and they even refer to polymorphic types. I could iron over this polymorphism by using either void pointers or some kind of limited type-erasure mechanism.

Alternatively, I could try to find the simplest path to some kind templating system, likely based on Terra. Ultimately if the event loop is implemented in Rust, the FFI will have to be monomorphic. LibUV is a very successful high-performance event loop library, and it is written in C so it must also be monomorphic.

I haven't considered how an event stream gets hooked into the core event loop. Should every event stream be immediately hooked in upon creation? This is okay as long as they are destroyed along with the `def` that introduced them.


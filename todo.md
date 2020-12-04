
# Event streams

Can't really do live rendering/game examples without either hacking a limited example together (like a processing-style environment), or without building in support for event streams.

I don't know how the event streams are supposed to work yet. This is really the next big problem to solve. The plan was to base them on the Observable design, because that seems fairly simple.

There has to be a central event loop, and it needs to find the event streams when some code is loaded. It needs to be able to push events to functions that are observing them.

Plan:

* Have an event type (struct of pointers to functions/state/whatever)
  * source state (e.g. sdl reference)
  * poll function
* Have a built-in function for creating event streams
  * it will be found on the env
  * in future it can change as the local context changes
* Have an observe function for registering listeners

## How do I do this without generics?

The function for creating event streams is going to be monomorphic.
The observe function is going to be monomorphic.

Maybe it's best to make the ugly version, and then fix it.

## Should I try and do it inside Regen?

I could move the watcher loop into Regen. It would mean recreating the watcher code in Regen, which I did in a previous prototype.

Advantages:
  * easier to call the handlers
  * easier to think about building nested environments/graphs into the language

Disadvantages:
  * it's still much harder to write code in Regen than in Rust
  * i'll have more code to update when i change the language
  * it will run slower
  * have to port the watcher code before even starting

## Hotloading Backlog

* Prevent compiler from panicking (it should fail gracefully)
  * Just run it in a thread? (i think panicking only kills threads)

* Replace event polling loops with event streams

* Free memory of old/deleted nodes
  * Note that SrcLocation structs reference the full source for a file
    * Replace all expr nodes and delete old source strings



# Expressing a simple game

Key questions to answer:

* How do we express that changing the render logic does not require any recalculation of game state?
* How do we define a self-caching stream?
* How are state reducers modeled? Is there actually a glitching issue?
* How is any of this shit going to get me to the Bret Victor demo?
* Is there actually a coherent design somewhere in this mess?

# Glitches and responsiveness

```c#
var a =
    Observable.Timer(TimeSpan.Zero, TimeSpan.FromMilliseconds(500))
    .Take(10);
var b = a.Scan(0L, (s, v) => s + v);
var c = a.Scan(0L, (s, v) => s - v);
var d = b.CombineLatest(c, (v1, v2) => v1 + v2);
var glitch = d.Scan(0L, (s, v) => s + Math.Abs(v));
glitch.ForEachAsync(v => {
    Console.WriteLine("accumulated glitch: {0}", v);
}).Wait();
```

The value of `d` seems like it should always be 0, but in fact it can glitch when `d` is updated between the updates of `b` and `c`. These glitches are corrected in `d`, but they are accumulated permanently in `e`.

For example, a short execution trace produced glitches immediately:

```
  b: 1
  c: -1
  d: 1
  glitch: 1
  d: 0
  glitch: 1
  b: 3
  c: -3
  d: -2
  glitch: 3
  d: 0
  glitch: 3
```

This can be fixed by using the `Zip` operator instead of `CombineLatest`, but `Zip` does not have the right semantics for responsive feedback. Zip has to wait for an event from both input streams before it can update, which could cause long delays and hugely increased memory usage:

```c#
var a = Observable.Timer(Zero, FromSeconds(1)).Take(10);
var b = Observable.Timer(Zero, FromMilliseconds(100)).Take(100);
var c = a.CombineLatest(b, (v1, v2) => v1 + v2);
var d = a.Zip(b, (v1, v2) => v1 + v2);
```

The `c` and `d` streams produce very different output, and `d` never uses most of the values from the `b` stream. `d` only produces one value per second, while `c` produces a value every 100 milliseconds.

To fix the glitches, we have to use something like `CombineLatest`, but impose topological ordering on the evaluation of stream operations.

## Evaluating streams as lists

For streams to be treated as lists, it's vital that some kind of time index is stored alongside each element. The `CombineLatest` operation is really a sort of temporal zip:

```csharp
List<(timestamp, (A, B))> TemporalZip<A, B>(
  List<(timestamp, A)> a,
  List<(timestamp, B)> b);
```

## Exposing in language

I think the Jonas Gebhardt example avoided the need for manually lifting things and wiring them together. Instead, the default operation was similar to `CombineLatest`. We could provide syntax like this:

```
def a = time_stream(interval: 100ms);
def b = state(0, fun(v : ptr i64) {
  *v += a;
});
def c = state(0, fun(v : ptr i64) {
  *v -= a
});
def d = state(0, fun(v : ptr i64) {
  *v += b + c
});
```

Is this too limiting? Too prescriptive?
  * The idea is that it's just a state container.
  * A state update probably should not always trigger immediate processing
  * Short term goal is just to make mutation possible again
  * Maybe leaving a value calculated from an old state is okay, as long as the eventual update to that value is deterministic, thanks to the global ordering

What would I actually _want_ the code of a game to look like?

# Implementing event streams

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


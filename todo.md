
# Types, Macros and staged metaprogramming

```

(def foo (fun () (do

)))
```

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


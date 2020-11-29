
# Hotloading TODO

## Critical

* ~~receive file change event~~
* ~~parse file's code to node tree~~
* mark nodes as either modified, new or deleted
  * new defs
  * removed defs
  * how should non-defs be handled?
* make list of nodes to recalculate
  * all modified nodes
  * any node that depends on a modified node, recursively
    * find dependencies of the modified nodes
      * turn node tree into structured expression tree
      * find all of the global references
      * check graph has no cycles with tarjan's algorithm
* unload deleted nodes and modified nodes
* evaluate new nodes and and modified nodes

## Secondary

* Prevent compiler from panicking (it should fail gracefully)
  * Just run it in a thread? (i think panicking only kills threads)
* Replace event polling loops with event streams
* Free memory of old/deleted nodes
  * Note that SrcLocation structs reference the full source for a file
    * Replace all expr nodes and delete old source strings


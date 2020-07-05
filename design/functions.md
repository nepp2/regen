
How are functions called?

Currently there is only one function. It has no parameters. It can see no globals.

A function should be able to see an environment containing globals. It obtains their pointer using a symbol. Is this at runtime, or is the pointer baked into the bytecode? For now it's easier to do at runtime.

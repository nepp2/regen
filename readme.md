# Regen

This repository contains the compiler & runtime for the [Regen programming language](https://regen.live).

## Note on safety & code quality

In an idiomatic Rust codebase, unsafe code is hidden behind safe APIs that have been carefully scrutinised and tested.

However, this is a research project undergoing constant iteration on features which require unsafe code, so the unsafe code in this codebase will not be tidied up until it has stabilised.

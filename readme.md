# Regen

This repository contains the compiler & runtime for the Regen programming language.

Regen is an research prototype aiming to provide a reliable feedback loop for general-purpose procedural programming, compatible with high-performance domains like game development.

It is intended to support a notebook UI in which definitions form a dataflow graph. This means the notebook can track which outputs are out of date and recalculate them automatically, without restarting the whole program. Persistent state is modelled with event-sourced state containers. A program may contain multiple state containers, which behave like any other graph node.

# Project structure

## compiler

This directory contains the regen compiler and runtime

## cli

A CLI interface is implemented in this directory. It is separated so that `compiler` does not depend on filesystem functions, and can eventually support wasm target.

## examples

This directory contains regen code samples. They are currently just snippets used during development to test out features and ideas in the language, rather than real examples.

## regen_libs

This directory contains a few core Regen libraries

## shared_libs

This directory contains a few shared libraries used by Regen sample code (currently just Windows DLLs).

## wasm

Wasm support is not yet implemented. This directory just contains a basic attempt to test Rust's wasm support.

# Compatibility

This prototype is currently only compatible with Windows. This is just because the C FFI is very basic, and currently makes Windows-specific assumptions.

# Note on safety

This is not an idiomatic Rust codebase. Usually unsafe code is hidden behind safe APIs that have been carefully scrutinised and tested. However, this is a research project undergoing constant iteration on features which require unsafe code, so some of the unsafe code won't be tidied up until it has stabilised.

The root issue is that the Regen memory model is more permissive than the Rust memory model, so the compiler can't efficiently interpret/manipulate Regen data structures without unsafe code. However, this unsafe code could still be cleaned up and centralised.

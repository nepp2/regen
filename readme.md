# Regen

This repository contains the compiler & runtime for the [Regen programming language](https://regen.live).

# Project structure

## regen_core

This directory contains the regen compiler and runtime

## regen_cli

A CLI interface is implemented in this directory. It is separated so that `regen_core` does not depend on filesystem functions, and can eventually support wasm target.

## examples

This directory contains regen code samples. They are currently just snippets used during development to test out features and ideas in the language, rather than real examples.

## regen_wasm

Wasm support is not yet implemented. This directory just contains a basic attempt to test Rust's wasm support.

# Note on safety & code quality

This is not an idiomatic Rust codebase. Usually unsafe code is hidden behind safe APIs that have been carefully scrutinised and tested. However, this is a research project undergoing constant iteration on features which require unsafe code, so the unsafe code in this codebase will not be tidied up until it has stabilised.

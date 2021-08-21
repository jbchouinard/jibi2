[![Rust](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml/badge.svg)](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml)

# jibi2

A re-implementation of [jibi](https://github.com/jbchouinard/jibi) with a bytecode
compiler and stack-based VM interpreter instead of a tree-walk interpreter.

The overall design and some code was inspired by
[Crafting Interpreters](https://craftinginterpreters.com),
however a lot of implementation details are different because of differences between
Rust and C.

---

Copyright 2021 Jérome Boisvert-Chouinard

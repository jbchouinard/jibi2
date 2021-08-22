[![Rust](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml/badge.svg)](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml)

# jibi2

A re-implementation of [jibi](https://github.com/jbchouinard/jibi) with a stack-based
bytecode compiler and VM instead of an AST interpreter, so that maybe it won't be
horribly, horribly slow.

The overall design and some of the code was inspired by
[Crafting Interpreters](https://craftinginterpreters.com),
however a lot of implementation details are different because of differences between
Rust and C.

---

Copyright 2021 Jérome Boisvert-Chouinard

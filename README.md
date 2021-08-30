[![Rust](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml/badge.svg)](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml)

# jibi2

A re-implementation of [jibi](https://github.com/jbchouinard/jibi) with a stack-based
bytecode compiler and VM with tail call optimization, instead of an AST interpreter,
so that it's not horribly slow, and doesn't blow the stack on recursive function calls.

The overall design and some of the code was inspired by
[Crafting Interpreters](https://craftinginterpreters.com),
however a lot of implementation details are different because of differences between
Rust and C, and between jibi2 and Lox.

---

Copyright 2021 Jérome Boisvert-Chouinard

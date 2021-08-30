[![Rust](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml/badge.svg)](https://github.com/jbchouinard/jibi2/actions/workflows/rust.yml)

# jibi2

A re-implementation of [jibi](https://github.com/jbchouinard/jibi) with a stack-based
bytecode compiler and VM instead of an AST interpreter, so that it's not horribly slow.

The overall design and some of the code was inspired by
[Crafting Interpreters](https://craftinginterpreters.com),
however a lot of implementation details are different because of differences between
Rust and C, and between jibi2 and Lox.

I also added some optimizations that are not in Crafting Interpreters:
- Tail call optimization (modify the top call frame when possible instead of pushing a new one,
  mainly to prevents stack overflow in tail recursive functions)
- Constants re-use (use same constant slot when multiple literals of same value appear)
- Compile-time evaluation of static expressions (e.g. (+ 12 10) compiles to constant 22)

---

Copyright 2021 Jérome Boisvert-Chouinard

#!/bin/bash
set -eu

echo "fibonacci(35)"
# echo "jibi"
# time jibi benchmarks/fibonacci.jibi
# echo "-----------------------------"
echo "jibi2 (gnu)"
TARGET=x86_64-unknown-linux-gnu make build
time target/x86_64-unknown-linux-gnu/release/jibi2 benchmarks/fibonacci.jibi
echo "-----------------------------"
echo "jibi2 (musl)"
TARGET=x86_64-unknown-linux-musl make build
time target/x86_64-unknown-linux-musl/release/jibi2 benchmarks/fibonacci.jibi
echo "-----------------------------"
echo "python3"
time python3 benchmarks/fibonacci.py
echo "-----------------------------"

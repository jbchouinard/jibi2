#!/bin/bash
set -eu

echo "fibonacci(35)"
echo "jibi"
time jibi benchmarks/fibonacci.jibi
echo "-----------------------------"
echo "jibi2"
time jibi2 benchmarks/fibonacci.jibi
echo "-----------------------------"
echo "python3"
time python3 benchmarks/fibonacci.py
echo "-----------------------------"

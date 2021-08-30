def fib(a, b, n):
    if n == 1:
        return a
    elif n == 2:
        return b
    else:
        return fib(n - 1) + fib(n - 2)


fib(1, 1, 35)

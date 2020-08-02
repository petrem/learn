from functools import wraps


def fib0(n):
    if n in (0, 1):
        return n
    return fib0(n - 1) + fib0(n - 2)


def memoize(f):
    cache = {0: 0, 1: 1}

    @wraps(f)
    def wrapped(n):
        if n not in cache:
            print(f"cache miss {n}")
            cache[n] = f(n)
        return cache[n]

    setattr(wrapped, "cache", cache)

    return wrapped


@memoize
def fib1(n):
    return fib1(n - 1) + fib1(n - 2)


def fib2(n):
    if n in (0, 1):
        return n
    prev = [0, 1]
    for _ in range(n - 2):
        prev[:] = [prev[-1], sum(prev)]
    return sum(prev)


def fib3(n):
    if n in (0, 1):
        return n
    prev = [0, 1]
    for _ in range(n - 2):
        s = prev[0] + prev[1]
        prev[0] = prev[1]
        prev[1] = s
    return prev[0] + prev[1]


_IMPLEMENTATIONS = [fib0, fib1, fib2, fib3]

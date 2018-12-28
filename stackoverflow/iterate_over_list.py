from __future__ import print_function
from collections import deque
from itertools import islice
from timeit import repeat

import numpy as np


list_ = [5, 'cat', 0xDEADBEEF, 4.0]
list_3k = list_ * 3000


def func(x, y):
    return x + 1


def f1():
    """list slice"""
    result = 0
    for offset in range(len(list_)):
        for elem in list_[offset:]:
            result = func(result, elem)
    return result


def f2():
    """deque"""
    dq = deque(list_)
    result = 0
    for i in range(len(dq)):
        for elem in dq:
            result = func(result, elem)
        dq.popleft()
    return result


def f3():
    """itertools slice"""
    result = 0
    for offset in range(len(list_)):
        for elem in islice(list_, offset, None):
            result = func(result, elem)
    return result


def f4():
    """basics"""
    result = 0
    n = len(list_)
    for offset in range(n):
        j = offset
        while j < n:
            result = func(result, list_[j])
            j += 1
    return result


def timeit(fn, number):
    print("{}: {} loops".format(fn.__name__, fn()))
    times = repeat(fn, repeat=3, number=number)
    print("{:.3f}s ± {:.3f}ms".format(np.mean(times), np.std(times)*1000))


if __name__ == "__main__":
    fs = [f1, f2, f3, f4]

    for f in fs:
        timeit(f, number=1000000)

    list_ = list_3k
    print()

    for f in fs:
        timeit(f, number=3)

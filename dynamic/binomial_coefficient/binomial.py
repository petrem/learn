def factorial(n):
    r = 1
    for i in range(2, n + 1):
        r *= i
    return r


def binomial0(n, k):
    return factorial(n) // (factorial(n - k) * factorial(k))


def binomial1(n, k):
    if n == 0 or k == 0 or k == n:
        return 1
    return binomial1(n - 1, k) + binomial1(n - 1, k - 1)


def binomial2(n, k):
    rows = [[1] * (n + 1) for _ in range(n + 1)]
    for i in range(1, n + 1):
        for j in range(1, i):
            rows[i][j] = rows[i - 1][j - 1] + rows[i - 1][j]
    return rows[n][k]


def binomial3(n, k):
    row = [1, 1]
    for i in range(2, n + 1):
        next_ = [1, *(row[j - 1] + row[j] for j in range(1, i)), 1]
        row = next_
    return row[k]


_IMPLEMENTATIONS = [binomial0, binomial1, binomial2, binomial3]
_NOT_SO_SLOW_IMPLEMENTATIONS = [binomial0, binomial2, binomial3]

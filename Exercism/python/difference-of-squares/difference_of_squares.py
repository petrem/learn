def square_of_sum(count):
    return sum(range(1, count + 1)) ** 2


def sum_of_squares(count):
    return sum(i ** 2 for i in range(1, count + 1))


def difference1(count):
    return square_of_sum(count) - sum_of_squares(count)


def difference(count):
    _sum = 0
    half_diff = 0
    for i in range(1, count):
        _sum += i
        half_diff += _sum * (i + 1)
    return 2 * half_diff

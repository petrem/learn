# https://stackoverflow.com/questions/12523586/python-format-size-application-converting-b-to-kb-mb-gb-tb
import math as m


MULTIPLES = ["B", "k{}B", "M{}B", "G{}B", "T{}B", "P{}B", "E{}B", "Z{}B", "Y{}B"]


def humanbytes(i, binary=False, precision=2):
    base = 1024 if binary else 1000
    multiple = m.trunc(m.log2(i) / m.log2(base))
    value = i / m.pow(base, multiple)
    suffix = MULTIPLES[multiple].format("i" if binary else "")
    return f"{value:.{precision}f} {suffix}"


if __name__ == "__main__":
    sizes = [
        1, 1024, 500000, 1048576, 50000000, 1073741824, 5000000000,
        1099511627776, 5000000000000]

    for i in sizes:
        print(f"{i} == {humanbytes(i)}, {humanbytes(i, binary=True)}")

from itertools import product


def generate_table(nvars):
    for t in product(*([(False, True)] * nvars)):
        yield t


def compare(f1, f2, nvars):
    for line in generate_table(nvars):
        r1 = f1(*line)
        r2 = f2(*line)
        #if r1 != r2:
        print line, '->', 'f1:', r1, 'f2:', r2


def orig(a, b, c):
    return (a and not b) or (a and b and c)


def derived(a, b, c):
    # return a and ((not b or a) and (not b or c))
    return a and (not b or c)


if __name__ == "__main__":
    compare(orig, derived, 3)

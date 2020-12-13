from memory_profiler import profile


class Data:
    @profile
    def __init__(self, name):
        self._name = name
        self._blob = list(range(1_000_000))

    def __str__(self):
        return self._name

    def __del__(self):
        print(f"{self} got deleted")


@profile
def fn1(data):
    a = [1, 2, 3, Data("in fn1's list")]
    b = [2, 3, 4]
    c = a + b
    c.append(data)
    return fn2(Data("fn2's argument"))


@profile
def fn2(l):
    return l



@profile
def main():
    fn1(Data("fn1's argument"))


if __name__ == "__main__":
    main()

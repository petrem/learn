from contextlib import contextmanager


def target_fn(x):
    print(f"do stuff with {x}")


@contextmanager
def ctx_inner(x):
    some_data = f"inner: {x}"
    print(f"ctx_inner: start")
    yield some_data
    target_fn(x)
    print(f"ctx_inner: stop")


@contextmanager
def ctx_outer(x):
    some_data = f"outer: {x}"
    print(f"ctx_outer: start")
    with ctx_inner(some_data) as data:
        yield data
    print(f"ctx_outer: stop")


if __name__ == "__main__":
    with ctx_inner("foo") as data:
        print(f"i'll do stuff with {data}")

    with ctx_outer("bar") as data:
        print(f"i'll do stuff with {data}")



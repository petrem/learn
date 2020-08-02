import pytest

from fib import _IMPLEMENTATIONS


@pytest.fixture(params=_IMPLEMENTATIONS, ids=[f.__name__ for f in _IMPLEMENTATIONS])
def implementation(request):
    return request.param


@pytest.mark.parametrize(
    "n,expected",
    [
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8),
        (7, 13),
        (8, 21),
        (9, 34),
        (10, 55),
        (11, 89),
        (12, 144),
    ]
)
def test_implementation_is_correct(implementation, n, expected):
    assert implementation(n) == expected


def test_implementation_performance(implementation, benchmark):
    def setup():
        if hasattr(implementation, "cache"):
            implementation.cache.clear()
            implementation.cache.update({0: 0, 1: 1})

    N = 400 # higher will exceed recursion depth
    result = benchmark.pedantic(implementation, args=(N,), setup=setup, rounds=2000)
    assert result == 176023680645013966468226945392411250770384383304492191886725992896575345044216019675

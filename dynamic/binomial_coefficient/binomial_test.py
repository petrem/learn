import pytest

from binomial import _IMPLEMENTATIONS, _NOT_SO_SLOW_IMPLEMENTATIONS
from binomial import factorial


@pytest.mark.parametrize("n,expected", [(1, 1), (2, 2), (3, 6), (4, 24), (10, 3628800)])
def test_factorial(n, expected):
    assert factorial(n) == expected

@pytest.fixture(params=_IMPLEMENTATIONS, ids=[f.__name__ for f in _IMPLEMENTATIONS])
def implementation(request):
    return request.param


@pytest.fixture(
    params=_NOT_SO_SLOW_IMPLEMENTATIONS,
    ids=[f.__name__ for f in _NOT_SO_SLOW_IMPLEMENTATIONS],
)
def not_so_slow_implementation(request):
    return request.param


@pytest.mark.parametrize(
    "n,k,expected",
    [
        (1, 0, 1),
        (1, 1, 1),
        (2, 0, 1),
        (2, 1, 2),
        (3, 0, 1),
        (3, 1, 3),
        (3, 2, 3),
        (3, 1, 3),
        (4, 0, 1),
        (4, 1, 4),
        (4, 2, 6),
        (4, 3, 4),
        (5, 0, 1),
        (5, 1, 5),
        (5, 2, 10),
        (5, 3, 10),
        (5, 4, 5),
        (5, 5, 1),
        (6, 0, 1),
        (6, 1, 6),
        (6, 2, 15),
        (6, 3, 20),
        (6, 4, 15),
        (6, 5, 6),
        (6, 6, 1),
        (7, 0, 1),
        (7, 1, 7),
        (7, 2, 21),
        (7, 3, 35),
        (7, 4, 35),
        (7, 5, 21),
        (7, 6, 7),
        (7, 7, 1),
        (8, 0, 1),
        (8, 1, 8),
        (8, 2, 28),
        (8, 3, 56),
        (8, 4, 70),
        (8, 5, 56),
        (8, 6, 28),
        (8, 7, 8),
        (8, 8, 1),
    ]
)
def test_implementation_is_correct(implementation, n, k, expected):
    assert implementation(n, k) == expected


@pytest.mark.parametrize(
    "n,k,expected",
    [
        (44, 3, 13244),
        (77, 17, 49053802362729780),
    ]
)
def test_higher_values(
    not_so_slow_implementation, n, k, expected
):
    assert not_so_slow_implementation(n, k) == expected


def test_implementation_performance(not_so_slow_implementation, benchmark):
    result = benchmark(not_so_slow_implementation, 500, 59)
    assert result == 354357969941112365641088578067163081684507859659277532204246140396804659972000

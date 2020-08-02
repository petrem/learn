from hypothesis import given
from hypothesis.strategies import composite, integers, text
import pytest

from .edit_distance import _IMPLEMENTATIONS
from .operations import edit_string, Operations


@pytest.fixture(
    scope="module", params=_IMPLEMENTATIONS, ids=[f.__name__ for f in _IMPLEMENTATIONS]
)
def implementation(request):
    return request.param


@pytest.fixture(
    scope="module",
    params=[f for f in _IMPLEMENTATIONS if not f.slow],
    ids=[f.__name__ for f in _IMPLEMENTATIONS if not f.slow],
)
def non_slow_implementation(request):
    return request.param


@pytest.mark.parametrize(
    "pattern,text,expected",
    [
        # identical strings
        ("", "", 0),
        ("a", "a", 0),
        ("abcd", "abcd", 0),
        ("a", "", 1),
        # same operation
        ("abcd", "", 4),
        ("", "a", 1),
        ("", "abcd", 4),
        ("a", "abcd", 3),
        ("abc", "d", 3),
        ("abcd", "a", 3),
        ("ab", "cab", 1),
        # more operations
        ("ab", "ba", 2),
        ("ab", "bac", 2),
        ("abb", "cab", 2),
        ("cba", "abc", 2),
        ("abc", "cba", 2),
        ("abc", "bbax", 3),

    ]
)
def test_varied_short_strings(implementation, pattern, text, expected):
    assert implementation(pattern, text) == expected


@pytest.mark.parametrize(
    "pattern,text,expected",
    [
        ("ceausescu", "iliescu", 5),
        ("you should not", "thou shalt not", 5),
        ("pedicabo ego vos et irumabo", "shut your fucking face, uncle fucker", 31),
        ("pedicabo ego vos et irrumabo", "shut your fucking face, uncle fucker", 31),
        (
            "how much wood would a woodchuck chuck if a woodchuck could chuck wood",
            "a woodchuck would chuck no amount of wood since a woodchuck can't chuck wood",
            37,
        )
    ]
)
def test_longer_strings(non_slow_implementation, pattern, text, expected):
    assert non_slow_implementation(pattern, text) == expected


# invariant examples mostly from
# https://stackoverflow.com/questions/36994342/generate-test-cases-for-levenshtein-distance-implementation-with-quickcheck

_HYPOTESIS_MAX_TEXT_SIZE = 5

# Levenshtein distance is a metric: symmetric, reflexive
# and respects the triangle inequality

@given(text(max_size=_HYPOTESIS_MAX_TEXT_SIZE))
def test_reflexivity(implementation, s):
    """The edit distance between any string and itself is 0."""
    assert implementation(s, s) == 0


@given(text(max_size=_HYPOTESIS_MAX_TEXT_SIZE), text(max_size=_HYPOTESIS_MAX_TEXT_SIZE))
def test_symmetry(implementation, p, s):
    """The edit distance between any string and itself is 0."""
    assert implementation(p, s) == implementation(s, p)


@given(
    text(max_size=_HYPOTESIS_MAX_TEXT_SIZE),
    text(max_size=_HYPOTESIS_MAX_TEXT_SIZE),
    text(max_size=_HYPOTESIS_MAX_TEXT_SIZE),
)
def test_symmetry(implementation, a, b, c):
    """The edit distance between any string and itself is 0."""
    assert implementation(a, b) + implementation(b, c) >= implementation(a, c)


@given(text(max_size=_HYPOTESIS_MAX_TEXT_SIZE), text(max_size=_HYPOTESIS_MAX_TEXT_SIZE))
def test_no_distance_is_negative(implementation, p, s):
    """No two strings have a negative edit distance."""
    assert implementation(p, s) >= 0


@composite
def text_and_index(draw):
    xs = draw(text(min_size=1, max_size=_HYPOTESIS_MAX_TEXT_SIZE))
    i = draw(integers(min_value=0, max_value=len(xs) - 1))
    return xs, i


@given(text_and_index())
def test_distance_of_one_edit_is_one(implementation, s_and_pos):
    """For an arbitrary string x, if you apply exactly one change to it, producing y,
    the edit distance between x and y should be 1.

    Note this assumes cost of operations is one
    """
    s, pos = s_and_pos
    assert implementation(s, edit_string(s, pos, Operations.INSERT, "x")) == 1
    assert implementation(s, edit_string(s, pos, Operations.DELETE)) == 1
    expect = 0 if s[pos] == "x" else 1
    assert implementation(s, edit_string(s, pos, Operations.REPLACE, "x")) == expect



def test_distance_differs_by_at_most_one_after_edit(implementation):
    """Given two strings x and y, compute the distance d between them.
    Then, change y, yielding y', and compute its distance from x:
    it should differ from d by at most 1.
    """
    pass


def test_distance_of_n_operations_is_at_most_n(implementation):
    """After applying n edits to a string x, the distance between the edited string
    and x should be at most n.
    """
    pass

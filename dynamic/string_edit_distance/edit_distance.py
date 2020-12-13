from functools import wraps

from .boundary_matrix import BoundaryMatrix
from .operations import delete_cost, insert_cost, match_cost


_IMPLEMENTATIONS = []


def _implementation(slow=False):

    def wrapper(f):

        @wraps(f)
        def wrapped(*args, **kwargs):
            return f(*args, **kwargs)

        _IMPLEMENTATIONS.append(wrapped)
        setattr(wrapped, "slow", slow)
        return wrapped

    return wrapper


@_implementation(slow=True)
def levenshtein0(pattern, text):
    if not pattern:
        return sum(insert_cost(c) for c in text)
    if not text:
        return sum(delete_cost(c) for c in pattern)
    return min(
        # match or substitute last char
        levenshtein0(pattern[:-1], text[:-1]) + match_cost(pattern[-1], text[-1]),
        # insert a character
        levenshtein0(pattern[:-1], text) + insert_cost(pattern[-1]),
        # delete a character
        levenshtein0(pattern, text[:-1]) + delete_cost(text[-1])
    )


@_implementation()
def levenshtein1(pattern, text):
    if not pattern:
        return sum(insert_cost(c) for c in text)
    if not text:
        return sum(delete_cost(c) for c in pattern)
    dists = BoundaryMatrix(pattern, text)
    for i, ch_pattern in enumerate(pattern):
        for j, ch_text in enumerate(text):
            possibilities = (
                dists[i - 1, j - 1] + match_cost(ch_text, ch_pattern),
                dists[i, j - 1] + insert_cost(ch_text),
                dists[i - 1, j] + delete_cost(ch_pattern),
            )
            dists[i, j] = min(possibilities)
    return dists[len(pattern) - 1, len(text) - 1]


def edit_distance(strategy, pattern, text):
    pass

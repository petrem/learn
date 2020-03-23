"""Generate all subsets from a given iterable."""
from itertools import compress

from backtrack import backtrack


def subsets(iterable):
    elements = list(iterable)

    def is_solution(a, context):
        return len(a) == context

    def generate_candidates(a, context):
        return (False, True)

    for selector in backtrack(
        is_solution=is_solution,
        generate_candidates=generate_candidates,
        context=len(elements)
    ):
        yield set(compress(elements, selector))

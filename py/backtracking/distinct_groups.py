"""Generate all subsets from a given iterable."""

from backtrack import backtrack
from subsets import subsets


def distinct_groups(iterable):
    all_elements = list(iterable)
    distinct_elements = set(all_elements)
    # the possible groups are the subsets of the multiset
    possible_groups = list(subsets(distinct_elements))
    possible_groups.remove(set())  # exclude the empty group

    print("\n\nPossible groups:", possible_groups)

    left = list(all_elements)  # make a copy

    def is_solution(solution, left):
        return len(left) == 0

    def generate_candidates(solution, left):
        return (group for group in possible_groups if set(left).issuperset(group))

    def make_move(solution, group, left):
        # print("make_move:", solution, group, left)
        for book in group:
            left.remove(book)
        print("make_move:", solution, group, left)

    def unmake_move(solution, group, left):
        left.extend(group)

    for groups in backtrack(
        is_solution=is_solution,
        generate_candidates=generate_candidates,
        make_move=make_move,
        unmake_move=unmake_move,
        context=left,
    ):
        yield groups


if __name__ == "__main__":
    print(list(distinct_groups([1, 1, 2])))

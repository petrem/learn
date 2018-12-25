#!/usr/bin/env python3

from itertools import chain, dropwhile, takewhile
import math


MAX_PIECES = 10


def display_towers(towers):
    lines = []
    discs = len(towers[1])  # assumes empty slots are filled with zero
    disc_n_width = int(math.log10(discs)) + 1
    tower_width = 2 * discs + disc_n_width
    empty_slot = f"{'|':^{tower_width}}"
    for i in range(discs-1, -1, -1):
        line = " ".join(
            f"""{f"{wings}{disc:{disc_n_width}}{wings}":^{tower_width}}"""
            if disc else empty_slot
            for disc, wings in map(
                lambda t: (t[i], "-" * t[i]),
                (towers[j] for j in range(1, 4))))
        lines.append(line)
    lines.append("")
    return "\n".join(lines)


class DiscMovementError(Exception):
    pass


class DiscNotFoundError(DiscMovementError):
    pass


class TowerFullError(DiscMovementError):
    pass


class BadTowerError(DiscMovementError):
    pass


class CannotMoveLargerOnSmallerError(DiscMovementError):
    pass


def build_tower(source, cond, mid_element):
    itr = iter(source)
    return chain(
        takewhile(cond, itr),
        (mid_element, ),
        itr)


def _move_n_discs(towers, t1, t2, n):
    t3 = 6 - t2 - t1
    if n == 1:
        disc = next(dropwhile(lambda x: x == 0, towers[t1][::-1]))
        return {
            t1: list(build_tower(towers[t1], lambda x: x != disc, 0)),
            t2: list(build_tower(towers[t2], lambda x: x != 0, disc)),
            t3: list(towers[t3])
        }
    else:
        return _move_n_discs(
            _move_n_discs(
                _move_n_discs(
                    towers, t1, t3, n - 1),
                t1, t2, 1),
            t3, t2, n - 1)


def move_discs(towers, source, dest):
    def _check_tower(t, which):
        if t not in range(1, 4):
            raise BadTowerError(f"Bad argument {which}: {t}")
    _check_tower(source, "source")
    _check_tower(dest, "dest")
    return _move_n_discs(towers, source, dest, len(towers[source]))


def generate_initial(n):
    if n <= 0 or n > MAX_PIECES:
        raise ValueError(
            f"Discs must be more than one and less then {MAX_PIECES}, not {n}")
    return {
        1: list(range(n, 0, -1)),
        2: [0] * n,
        3: [0] * n,
    }


if __name__ == "__main__":
    towers = generate_initial(10)
    print(display_towers(towers))
    move_discs(towers, 1, 3)
    print(display_towers(towers))

#!/usr/bin/env python3

import itertools
import math


MAX_PIECES = 10


def display_towers(towers):
    discs = len(towers[0])  # assumes empty slots are filled with zero
    disc_n_width = int(math.log10(discs)) + 1
    tower_width = 2 * discs + disc_n_width
    empty_slot = f"{'|':^{tower_width}}"
    for i in range(discs-1, -1, -1):
        line = " ".join(
            f"""{f"{wings}{disc:{disc_n_width}}{wings}":^{tower_width}}"""
            if disc else empty_slot
            for disc, wings in map(lambda t: (t[i], "-" * t[i]), towers))
        print(line)
    print()


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


def _get_top_disc(towers, t):
    if t < 1 or t > 3:
        raise BadTowerError(f"No such tower {t}")
    try:
        return next(itertools.dropwhile(lambda x: x == 0, towers[t-1][::-1]))
    except StopIteration:
        raise DiscNotFoundError


def _remove_top_disc(towers, t):
    top = _get_top_disc(towers, t)
    towers[t-1][towers[t-1].index(top)] = 0


def _disc_index(tower, disc):
    # not used
    return tower.index(disc)


def _put_disc(towers, t, d):
    if t < 1 or t > 3:
        raise BadTowerError(f"No such tower {t}")
    try:
        top = _get_top_disc(towers, t)
        top_index = towers[t-1].index(top)
    except DiscNotFoundError:
        top = MAX_PIECES + 1
        top_index = -1
    if top_index + 1 >= len(towers[t-1]):
        raise TowerFullError(f"Tower {t} is full.")
    if d >= top:
        raise CannotMoveLargerOnSmallerError(f"{d} over {top}")
    towers[t-1][top_index+1] = d


def move_disc(towers, t1, t2):
    t1top = _get_top_disc(towers, t1)
    _put_disc(towers, t2, t1top)
    _remove_top_disc(towers, t1)


def move_discs(towers, source, dest):
    def _move_n_discs(t1, t2, n):
        if n == 1:
            move_disc(towers, t1, t2)
        else:
            t3 = 6 - t2 - t1
            _move_n_discs(t1, t3, n - 1)
            move_disc(towers, t1, t2)
            _move_n_discs(t3, t2, n - 1)
    n_discs = len(towers[source-1])
    _move_n_discs(source, dest, n_discs)


def generate_initial(n):
    if n <= 0 or n > MAX_PIECES:
        raise ValueError(
            f"Discs must be more than one and less then {MAX_PIECES}, not {n}")
    return [
        list(range(n, 0, -1)),
        [0] * n,
        [0] * n,
    ]


if __name__ == "__main__":
    towers = generate_initial(10)
    display_towers(towers)
    move_discs(towers, 1, 3)
    display_towers(towers)

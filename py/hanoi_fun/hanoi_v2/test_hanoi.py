from . import hanoi

from copy import deepcopy
import itertools

import pytest


def test_generate_initial():
    for value in [-1, 0, hanoi.MAX_PIECES + 1]:
        with pytest.raises(ValueError):
            hanoi.generate_initial(value)
    assert hanoi.generate_initial(1) == {
        1: [1],
        2: [0],
        3: [0]
    }
    assert hanoi.generate_initial(3) == {
        1: [3, 2, 1],
        2: [0] * 3,
        3: [0] * 3
    }


def test_display():
    towers = hanoi.generate_initial(10)
    golden = """\
         - 1-                    |                      |           
        -- 2--                   |                      |           
       --- 3---                  |                      |           
      ---- 4----                 |                      |           
     ----- 5-----                |                      |           
    ------ 6------               |                      |           
   ------- 7-------              |                      |           
  -------- 8--------             |                      |           
 --------- 9---------            |                      |           
----------10----------           |                      |           
"""
    assert hanoi.display_towers(towers) == golden


def test_build_tower():
    # remove last disc
    assert list(hanoi.build_tower([4, 3, 2, 0], lambda x: x != 2, 0)) == [4, 3, 0, 0]
    assert list(hanoi.build_tower([4, 3, 2, 1], lambda x: x != 1, 0)) == [4, 3, 2, 0]
    assert list(hanoi.build_tower([4, 0, 0, 0], lambda x: x != 4, 0)) == [0, 0, 0, 0]
    # add a disc
    assert list(hanoi.build_tower([4, 3, 2, 0], lambda x: x != 0, 1)) == [4, 3, 2, 1]
    assert list(hanoi.build_tower([4, 3, 0, 0], lambda x: x != 0, 2)) == [4, 3, 2, 0]
    assert list(hanoi.build_tower([0, 0, 0, 0], lambda x: x != 0, 4)) == [4, 0, 0, 0]


class TestMoveDiscs():
    def test_tower_of_one_disc(self):
        initial = hanoi.generate_initial(1)
        towers1 = hanoi.move_discs(initial, 1, 2)
        assert towers1 == {
            1: [0],
            2: [1],
            3: [0]
        }
        towers2 = hanoi.move_discs(towers1, 2, 3)
        assert towers2 == {
            1: [0],
            2: [0],
            3: [1]
        }
        towers3 = hanoi.move_discs(towers2, 3, 1)
        assert towers3 == {
            1: [1],
            2: [0],
            3: [0]
        }

    def test_tower_of_three_discs(self):
        initial = hanoi.generate_initial(3)
        towers = hanoi.move_discs(initial, 1, 3)
        assert towers == {
            1: [0, 0, 0],
            2: [0, 0, 0],
            3: [3, 2, 1]
        }

    def test_no_side_effects(self):
        initial = hanoi.generate_initial(3)
        towers0 = deepcopy(initial)
        hanoi.move_discs(towers0, 1, 2)
        assert initial == towers0

    def test_bad_input(self):
        initial = hanoi.generate_initial(3)
        for t1, t2 in map(
                itertools.permutations, zip(itertools.repeat(1), [-1, 0, 4])):
            with pytest.raises(hanoi.BadTowerError):
                hanoi.move_discs(initial, t1, t2)

from . import hanoi

from copy import deepcopy as copy
import pytest


def test_generate_initial():
    with pytest.raises(ValueError):
        hanoi.generate_initial(-1)
        hanoi.generate_initial(0)
        hanoi.generate_initial(hanoy.MAX_PIECES + 1)
    assert hanoi.generate_initial(1) == [[1], [0], [0]]
    assert hanoi.generate_initial(3) == [[3, 2, 1], [0]*3, [0]*3]


class TestMoveDisc():
    @classmethod
    def setup_class(cls):
        cls.init_one = hanoi.generate_initial(1)
        cls.init_two = hanoi.generate_initial(2)
        cls.mock_three = [[2, 1, 0], [3, 0, 0], [0, 0, 0]]
        cls.bad_towers = [-1, 0, 4]

    def test_get_top_disc(self):
        towers = copy(self.mock_three)
        assert hanoi._get_top_disc(towers, 1) == 1
        assert hanoi._get_top_disc(towers, 2) == 3
        for bad in self.bad_towers:
            with pytest.raises(hanoi.BadTowerError):
                hanoi._get_top_disc(towers, bad)

    def test_tower_full(self):
        towers = hanoi.generate_initial(1)
        with pytest.raises(hanoi.TowerFullError):
            hanoi._put_disc(towers, 1, 1)

    def test_put_disc(self):
        towers = copy(self.mock_three)
        for bad in self.bad_towers:
            with pytest.raises(hanoi.BadTowerError):
                hanoi._put_disc(towers, bad, 1)

    def test_remove_top_disc(self):
        towers = copy(self.init_two)
        hanoi._remove_top_disc(towers, 1)
        assert towers == [[2, 0], [0, 0], [0, 0]]
        hanoi._remove_top_disc(towers, 1)
        assert towers == [[0, 0], [0, 0], [0, 0]]
        with pytest.raises(hanoi.DiscNotFoundError):
            hanoi._remove_top_disc(towers, 1)
        for bad in self.bad_towers:
            with pytest.raises(hanoi.BadTowerError):
                hanoi._put_disc(towers, bad, 1)

    def test_move_with_one_disc(self):
        towers = copy(self.init_one)
        hanoi.move_disc(towers, 1, 2)
        assert towers == [[0], [1], [0]]
        with pytest.raises(hanoi.DiscNotFoundError):
            hanoi.move_disc(towers, 1, 2)

    def test_move_with_two_discs(self):
        towers = copy(self.init_two)
        hanoi.move_disc(towers, 1, 3)
        assert towers == [[2, 0], [0, 0], [1, 0]]
        with pytest.raises(hanoi.DiscNotFoundError):
            hanoi.move_disc(towers, 2, 3)
        with pytest.raises(hanoi.CannotMoveLargerOnSmallerError):
            hanoi.move_disc(towers, 1, 3)
        hanoi.move_disc(towers, 1, 2)
        assert towers == [[0, 0], [2, 0], [1, 0]]
        hanoi.move_disc(towers, 3, 2)
        assert towers == [[0, 0], [2, 1], [0, 0]]

    def test_move_wrong_towers(self):
        towers = copy(self.init_two)
        for good, bad in [
                (t1, t2)
                for bad in self.bad_towers
                for t1, t2 in ((1, bad), (bad, 1))]:
            with pytest.raises(hanoi.DiscMovementError):
                hanoi.move_disc(towers, good, bad)


class TestMoveDiscs():
    def test_tower_of_one_disc(self):
        towers = hanoi.generate_initial(1)
        hanoi.move_discs(towers, 1, 2)
        assert towers == [[0], [1], [0]]
        hanoi.move_discs(towers, 2, 3)
        assert towers == [[0], [0], [1]]
        hanoi.move_discs(towers, 3, 1)
        assert towers == [[1], [0], [0]]

    def test_tower_of_three_discs(self):
        towers = hanoi.generate_initial(3)
        hanoi.move_discs(towers, 1, 3)
        assert towers == [[0, 0, 0], [0, 0, 0], [3, 2, 1]]

from . import hanoy

from copy import deepcopy as copy
import pytest


def test_generate_initial():
    with pytest.raises(ValueError):
        hanoy.generate_initial(-1)
        hanoy.generate_initial(0)
        hanoy.generate_initial(hanoy.MAX_PIECES + 1)
    assert hanoy.generate_initial(1) == [[1], [0], [0]]
    assert hanoy.generate_initial(3) == [[3, 2, 1], [0]*3, [0]*3]


class TestMoveDisc():
    @classmethod
    def setup_class(cls):
        cls.init_one = hanoy.generate_initial(1)
        cls.init_two = hanoy.generate_initial(2)
        cls.mock_three = [[2, 1, 0], [3, 0, 0], [0, 0, 0]]
        cls.bad_towers = [-1, 0, 4]

    def test_get_top_disc(self):
        towers = copy(self.mock_three)
        assert hanoy._get_top_disc(towers, 1) == 1
        assert hanoy._get_top_disc(towers, 2) == 3
        for bad in self.bad_towers:
            with pytest.raises(hanoy.BadTowerError):
                hanoy._get_top_disc(towers, bad)

    def test_tower_full(self):
        towers = hanoy.generate_initial(1)
        with pytest.raises(hanoy.TowerFullError):
            hanoy._put_disc(towers, 1, 1)

    def test_put_disc(self):
        towers = copy(self.mock_three)
        for bad in self.bad_towers:
            with pytest.raises(hanoy.BadTowerError):
                hanoy._put_disc(towers, bad, 1)

    def test_remove_top_disc(self):
        towers = copy(self.init_two)
        hanoy._remove_top_disc(towers, 1)
        assert towers == [[2, 0], [0, 0], [0, 0]]
        hanoy._remove_top_disc(towers, 1)
        assert towers == [[0, 0], [0, 0], [0, 0]]
        with pytest.raises(hanoy.DiscNotFoundError):
            hanoy._remove_top_disc(towers, 1)
        for bad in self.bad_towers:
            with pytest.raises(hanoy.BadTowerError):
                hanoy._put_disc(towers, bad, 1)

    def test_move_with_one_disc(self):
        towers = copy(self.init_one)
        hanoy.move_disc(towers, 1, 2)
        assert towers == [[0], [1], [0]]
        with pytest.raises(hanoy.DiscNotFoundError):
            hanoy.move_disc(towers, 1, 2)

    def test_move_with_two_discs(self):
        towers = copy(self.init_two)
        hanoy.move_disc(towers, 1, 3)
        assert towers == [[2, 0], [0, 0], [1, 0]]
        with pytest.raises(hanoy.DiscNotFoundError):
            hanoy.move_disc(towers, 2, 3)
        with pytest.raises(hanoy.CannotMoveLargerOnSmallerError):
            hanoy.move_disc(towers, 1, 3)
        hanoy.move_disc(towers, 1, 2)
        assert towers == [[0, 0], [2, 0], [1, 0]]
        hanoy.move_disc(towers, 3, 2)
        assert towers == [[0, 0], [2, 1], [0, 0]]

    def test_move_wrong_towers(self):
        towers = copy(self.init_two)
        for good, bad in [
                (t1, t2)
                for bad in self.bad_towers
                for t1, t2 in ((1, bad), (bad, 1))]:
            with pytest.raises(hanoy.DiscMovementError):
                hanoy.move_disc(towers, good, bad)


class TestMoveDiscs():
    def test_tower_of_one_disc(self):
        towers = hanoy.generate_initial(1)
        hanoy.move_discs(towers, 1, 2)
        assert towers == [[0], [1], [0]]
        hanoy.move_discs(towers, 2, 3)
        assert towers == [[0], [0], [1]]
        hanoy.move_discs(towers, 3, 1)
        assert towers == [[1], [0], [0]]

    def test_tower_of_three_discs(self):
        towers = hanoy.generate_initial(3)
        hanoy.move_discs(towers, 1, 3)
        assert towers == [[0, 0, 0], [0, 0, 0], [3, 2, 1]]

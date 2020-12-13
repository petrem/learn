from abc import ABC, abstractmethod
from itertools import accumulate


class Strategy(ABC):

    def __init__(self, pattern, text):  # not needed
        self.matrix = BoundaryMatrix()

    @abstractmethod
    def init_boundary_row(self, text):
        raise NotImplementedError

    @abstractmethod
    def init_boundary_col(self, pattern):
        raise NotImplementedError

    @abstractmethod
    def insert_cost(self, *args):
        raise NotImplementedError

    @abstractmethod
    def delete_cost(self, *args):
        raise NotImplementedError

    @abstractmethod
    def match_cost(self, *args):
        raise NotImplementedError

    @abstractmethod
    def goal_cell(self, *args):
        raise NotImplementedError


class LevenshteinStrategy(Strategy):

    def init_boundary_row(self, text):
        return list(accumulate(map(self.insert_cost, text), initial=0))

    def init_boundary_col(self, pattern):
        return list(accumulate(map(self.delete_cost, pattern), initial=0))

    def insert_cost(self, *args):
        return 1

    def delete_cost(self, *args):
        return 1

    def match_cost(self, ch1, ch2, *args):
        return 0 if ch1 == ch2 else 1

    def goal_cell(self, pattern, text):
        return len(pattern) - 1, len(text) - 1

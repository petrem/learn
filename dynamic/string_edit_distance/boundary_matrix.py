from itertools import accumulate

from .operations import insert_cost, delete_cost


class BoundaryMatrix:
    def __init__(self, pattern, text):
        self._p_len = len(pattern)
        self._t_len = len(text)
        if self._p_len < 1 or self._t_len < 1:
            raise ValueError("Need at least one character in both pattern and text")
        self._m = [[0] * self._t_len for _ in pattern]
        self._boundary_row = list(accumulate(map(insert_cost, text), initial=0))
        self._boundary_col = list(accumulate(map(delete_cost, pattern), initial=0))
        # for printing
        self._max = max(max(self._boundary_col), max(self._boundary_row))

    def __getitem__(self, key):
        """Accepts a tuple representing the (extended) matrix coordinates."""
        if isinstance(key, tuple):
            row, col = key
            if -1 <= row < self._p_len and -1 <= col < self._t_len:
                if row == -1:
                    if col == -1:
                        return 0  # maybe use self._boundary_row[0]
                    return self._boundary_row[col + 1]
                if col == -1:
                    return self._boundary_col[row + 1]
                return self._m[row][col]
            raise IndexError(f"Index {row, col} out of bounds")
        raise TypeError(f"Cannot index by {type(key)}")

    def __setitem__(self, key, value):
        if isinstance(key, tuple):
            row, col = key
            if 0 <= row < self._p_len and 0 <= col < self._t_len:
                self._m[row][col] = value
                if value > self._max:
                    self._max = value
                return
            raise IndexError(f"Index {key} out of bounds")
        raise TypeError(f"Cannot index by {type(key)}")

    def __str__(self):
        width = len(f"{self._max}")
        return "\n".join(
            " ".join(f"{self[i, j]:{width}}" for j in range(-1, self._t_len))
            for i in range(-1, self._p_len)
        )

    @property
    def boundary_row(self):
        return self._boundary_row

    @property
    def boundary_col(self):
        return self._boundary_col

    @property
    def rows(self):
        return self._p_len

    @property
    def cols(self):
        return self._t_len

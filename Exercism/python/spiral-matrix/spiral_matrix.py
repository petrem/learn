from enum import Enum
from itertools import cycle
from typing import Iterable, Generator


def spiral_matrix(size):
    matrix = [[None] * size] * size
    total_numbers = size * size
    cycles = total_numbers / 3
    last = total_numbers % 3
    
    for direction, i in zip(directions, range(1, size * size + 1)):
        pass


def _triads(n):
    total = n * n
    side = n - 1
    directions = cycle([(0, 1), (1, 0), (0, -1), (-1, 0)])
    while True:
        if total > 3 * side:
            yield next(directions) * side
            total -= 3 * side
        

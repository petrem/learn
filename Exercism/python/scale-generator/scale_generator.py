from enum import IntEnum
from intertools import accumulate
from typing import List


class Scale:
    def __init__(self, tonic: str) -> None:
        self.tonic = Note(Tone(tonic), Degree.Tonic)

    def chromatic(self) -> List[str]:
        pass

    def interval(self, intervals: str) -> List[str]:
        pass


def modulo(n):
    def wrapper(cls):
        if not issubclass(cls, int):
            raise TypeError(f"{cls} must be a subclass of int")

        def add(self, other):
            return cls(int.__add__(self, other) % n)

        def sub(self, other):
            return cls(int.__sub__(self, other) % n)

        cls.__add__ = add
        cls.__sub__ = sub
        return cls
    return wrapper


@modulo(7)
class Degree(IntEnum):
    Tonic = 0
    Supertonic = 1
    Mediant = 2
    Subdominant = 3
    Dominant = 4
    Submediant = 5
    Leading = 6


@modulo(12)
class Tone(int):
    pass


class Mode:
    def __init__(self, interval_sequence):
        self._interval_sequence = interval_sequence
        self._tones = list(accumulate(interval_sequence))

    def tone(self, degree):
        return Tone(self._tones[degree])


class Note:
    def __init__(self, tone, degree):
        self.tone = tone
        self.degree = degree

    def __repr__(self):
        return f"Note({self.tone}, {self.degree})"

    def __str__(self):
        return ""

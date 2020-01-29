from __future__ import annotations

import re
from itertools import product
from math import ceil, sqrt
from typing import Tuple


def cipher_text(plain_text: str) -> str:
    normalized = re.sub(r"\W", "", plain_text).lower()
    if normalized:
        return _square_crypt(normalized)
    return ""


def _square_crypt(message: str) -> str:
    c, r = _get_cols_rows(len(message))
    padding = " " * (c * r - len(message))
    message += padding
    return " ".join(
        "".join(message[i * c + j] for i in range(r))
        for j in range(c)
    )


def _get_cols_rows(n: int) -> Tuple[int, int]:
    return min(
        (c, r)
        for c, r in product(_range_upto_sqrt(n), _range_upto_sqrt(n))
        if c >= r and c - r <= 1 and c * r >= n
    )


def _range_upto_sqrt(n: int) -> range:
    return range(1, ceil(sqrt(n)) + 1)

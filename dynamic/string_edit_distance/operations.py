from enum import IntEnum, auto


class Operations(IntEnum):
    INSERT = auto()
    DELETE = auto()
    REPLACE = auto()
    MATCH = REPLACE


def _insert(s, pos, ch):
    if pos == 0:
        return ch + s
    if 0 < pos <= len(s):
        return ch.join((s[:pos], s[pos:]))
    raise ValueError(f"Cannot insert in {s=} of length {len(s)} at {pos=}")


def _delete(s, pos, ch=None):
    if 0 <= pos < len(s):
        return "".join((s[:pos], s[pos + 1:]))
    raise ValueError(f"Cannot delete from {s=} of length {len(s)} at {pos=}")


def _replace(s, pos, ch):
    if 0 <= pos < len(s):
        return ch.join((s[:pos], s[pos + 1:]))
    raise ValueError(f"Cannot replace in {s=} of length {len(s)} at {pos=}")


def edit_string(s, pos, op, ch=None):
    return {
        Operations.INSERT: _insert,
        Operations.DELETE: _delete,
        Operations.REPLACE: _replace,
    }[op](s, pos, ch=ch)


def insert_cost(ch):  # noqa
    return 1


def delete_cost(ch):  # noqa
    return 1


def match_cost(ch1, ch2):
    if ch1 == ch2:
        return 0
    return 1

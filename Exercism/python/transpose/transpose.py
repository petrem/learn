from itertools import zip_longest


def transpose(lines: str) -> str:
    return "\n".join(
        "".join(l).rstrip(_SENTINEL).replace(_SENTINEL, " ")
        for l in zip_longest(*lines.splitlines(), fillvalue=_SENTINEL)
    )


_SENTINEL = "uFFFF"

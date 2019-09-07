import re


def abbreviate(words):
    return "".join(
        map(str.upper, re.findall(r"(?<![a-z'])([a-z])", words, re.I))
    )

import pytest

from .operations import edit_string, Operations


@pytest.mark.parametrize(
    "s,pos,ch,expected",
    [
        ("", 0, "a", "a"),
        ("abc", 0, "x", "xabc"),
        ("abc", 1, "x", "axbc"),
        ("abc", 2, "x", "abxc"),
        ("abc", 3, "x", "abcx"),
    ]
)
def test_insert(s, pos, ch, expected):
    assert edit_string(s, pos, Operations.INSERT, ch) == expected


@pytest.mark.parametrize(
    "s,pos",
    [
        ("", -1),
        ("", 1),
        ("abc", -1),
        ("abc", 4),
    ]
)
def test_insert_bad_input(s, pos):
    with pytest.raises(ValueError):
        edit_string(s, pos, Operations.INSERT, "x")


@pytest.mark.parametrize(
    "s,pos,expected",
    [
        ("abc", 0, "bc"),
        ("abc", 1, "ac"),
        ("abc", 2, "ab"),
    ]
)
def test_delete(s, pos, expected):
    assert edit_string(s, pos, Operations.DELETE) == expected


@pytest.mark.parametrize(
    "s,pos",
    [
        ("", -1),
        ("", 0),
        ("", 1),
        ("abc", -1),
        ("abc", 3),
    ]
)
def test_delete_bad_input(s, pos):
    with pytest.raises(ValueError):
        edit_string(s, pos, Operations.DELETE)


@pytest.mark.parametrize(
    "s,pos,ch,expected",
    [
        ("abc", 0, "x", "xbc"),
        ("abc", 1, "x", "axc"),
        ("abc", 2, "x", "abx"),
    ]
)
def test_replace(s, pos, ch, expected):
    assert edit_string(s, pos, Operations.REPLACE, ch) == expected


@pytest.mark.parametrize(
    "s,pos",
    [
        ("", -1),
        ("", 0),
        ("", 1),
        ("abc", -1),
        ("abc", 3),
    ]
)
def test_replace_bad_input(s, pos):
    with pytest.raises(ValueError):
        edit_string(s, pos, Operations.REPLACE, "x")

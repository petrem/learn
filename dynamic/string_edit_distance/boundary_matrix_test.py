import pytest

from .boundary_matrix import BoundaryMatrix


# Note these assume insert_cost, delete_cost are always 1


@pytest.mark.parametrize(
    "p,t,expected_p,expected_t",
    [
        ("a", "a", [0, 1], [0, 1]),
        ("a", "ab", [0, 1], [0, 1, 2]),
        ("ab", "a", [0, 1, 2], [0, 1]),
        ("abc", "xyz", [0, 1, 2, 3], [0, 1, 2, 3]),
    ]
)
def test_boundaries(p, t, expected_p, expected_t):
    dists = BoundaryMatrix(p, t)
    assert dists.boundary_row == expected_t, f"unexpected {dists.boundary_row=}"
    assert dists.boundary_col == expected_p, f"unexpected {dists.boundary_col=}"
    for i in range(-1, len(t)):
        assert dists[-1, i] == expected_t[i + 1], f"unexpected dists[-1, {i}] == {dists[-1, i]}"
    for j in range(-1, len(p)):
        assert dists[j, -1] == expected_p[j + 1], f"unexpected dists[{j}, -1] == {dists[j, -1]}"
    assert dists[-1, -1] == expected_t[0] == expected_p[0], f"unexpected dists[-1, -1] == {dists[-1, -1]}"


@pytest.fixture(scope="module")
def fixed_matrix():
    return BoundaryMatrix("ab", "xyz")


def test_simple_sizes(fixed_matrix):
    assert fixed_matrix.rows == 2
    assert fixed_matrix.cols == 3


@pytest.mark.parametrize(
    "r,c",
    [
        # boundaries
        (-2, 0), (-1, 3), (0, -2), (2, -1),
        # outside matrix
        (0, 3), (2, 0),
    ]
)
def test_cannot_access_outside_boundary(fixed_matrix, r, c):
    with pytest.raises(IndexError):
        _ = fixed_matrix[r, c]
    with pytest.raises(IndexError):
        fixed_matrix[r, c] = 1


def test_can_set_and_get(fixed_matrix):
    for i in range(fixed_matrix.rows):
        for j in range(fixed_matrix.cols):
            fixed_matrix[i, j] = i + j
    for i in range(fixed_matrix.rows):
        for j in range(fixed_matrix.cols):
            assert fixed_matrix[i, j] == i + j


def test_can_set_and_get_after_set(fixed_matrix):
    for i in range(fixed_matrix.rows):
        for j in range(fixed_matrix.cols):
            fixed_matrix[i, j] = i + j
    for i in range(fixed_matrix.rows):
        for j in range(fixed_matrix.cols):
            fixed_matrix[i, j] = i + j + 5
    for i in range(fixed_matrix.rows):
        for j in range(fixed_matrix.cols):
            assert fixed_matrix[i, j] == i + j + 5

import typing


Basket = typing.List[int]
GrouppedBooks = typing.Iterable[tuple]


def total(basket: Basket) -> int:
    return min(_discount(group) for group in _generate_book_groups(basket))


def _generate_book_groups(basket: Basket) -> GrouppedBooks:
    pass


def _discount(groupped_books: GrouppedBooks) -> int:
    return sum(len(t) * _DISCOUNTS[len(t)] for t in groupped_books)


_DISCOUNTS = {n: 800 - 8 * d for n, d in enumerate((0, 5, 10, 20, 25), start=1)}

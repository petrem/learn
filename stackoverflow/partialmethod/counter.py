from functools import partialmethod


class Counter:
    def __init__(self, initial):
        self._value = 0

    def __str__(self):
        return str(self._value)

    def increase(self, by):
        self._value += by

    # on descriptor (a method is a descriptor too, that is doing the "self" magic)
    increment = partialmethod(increase, 1)

    # on non-descriptor
    name = lambda self: f"Counter of {self}"
    increment2 = partialmethod(name)

    # partialmethod generator

    def increment_returner(self, by):
        return partialmethod(Counter.increase, by)


# partialmethod used as intended on methods:

c = Counter(0)
c.increment()
print(f"incremented counter: {c}")    # --> 1
print(f"c.increment: {c.increment}")  # --> functools.partial(<bound method Counter.increase of <__main__.Counter object at 0x108fa0610>>, 1)
print(f"c.increment has __call__: {hasattr(c.increment, '__call__')}")  # --> True

print()

# partialmethod used (as intended?), on non-descriptor callables
print(f"c.name() returns: {c.name()}")  # --> "Counter of 1"
print(f"c.name is: {c.name}")  # --> <bound method Counter.<lambda> of <__main__.Counter object at 0x10208dc10>>

print()

# a "partialmethod" generator

incrementer = c.increment_returner(2)
print(f"icrementer: {incrementer}")  # --> functools.partialmethod(<bound method Counter.increase of <__main__.Counter object at 0x104e74790>>, 2, )
print(f"incrementer has __call__: {hasattr(incrementer, '__call__')}")  # --> False
print(f"incrementer has __get__: {hasattr(incrementer, '__get__')}")  # --> True

incrementer.__get__(c, Counter)()
print(f"counter after 'simulating' python's magic: {c}")  # --> 3
print(f"'simulated' invocation of attribute lookup: {incrementer.__get__(c, Counter)}")  # --> functools.partial(<bound method Counter.increase of <__main__.Counter object at 0x10d7b7c50>>, 2)

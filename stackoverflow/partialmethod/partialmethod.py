from functools import partial, partialmethod


class AClass():
    def __init__(self, val):
        self.v = val

    def _fun(self, x):
        z = x + self.v  # some computation
        return z

    def fun1(self, x):
        def bound_fun_caller():
            return self._fun(x)
        return bound_fun_caller

    def fun2(self, x):
        # quite silly, but here it is
        return partialmethod(AClass._fun, x).__get__(self, AClass)

    def fun3(self, x):
        return partial(AClass._fun, self, x)

    # for completeness, binding to a known value
    plus_four = partialmethod(_fun, 4)

    def add_fun(self, name, x):
        # Careful, this might hurt a lot...
        setattr(AClass, name, partialmethod(AClass._fun, x))


a = AClass(10)

b1 = a.fun1(1)
print(b1())

b2 = a.fun2(2)
print(b2())

b3 = a.fun3(3)
print(b3())

print(a.plus_four())

a.add_fun("b5", 5)
print(a.b5())

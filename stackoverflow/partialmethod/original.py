from functools import partialmethod


class AClass():
    def __init__(self, val):
        self.v = val
    def _fun(self, x):
        z = x + self.v # some computation
        return z
    def _notfun(*args):
        print(",".join([str(a) for a in args]))
    def fun(self, x):
        return partialmethod(self._notfun, x)

a = AClass(10)
b = a.fun(1)
#print(b()) # TypeError: 'partialmethod' object is not callable
b.__get__(a, AClass)()

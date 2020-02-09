from inspect import getfullargspec
from functools import reduce
from math import inf


def arity(fn):
    """Try to determine the number of arguments of a callable.

    Some callables like reduce, map, etc. are not supported so we try again with
    their __call__ method.

    If they have varargs, we return infinity as they can get any number of arguments.
    """
    try:
        arg_spec = getfullargspec(fn)
    except TypeError:
        arg_spec = getfullargspec(fn.__call__)
    if arg_spec.varargs is not None:
        return inf
    return len(arg_spec.args)


def compose(*functions):
    assert all(arity(f) == 1 for f in functions)
    return reduce(lambda f, g: lambda a: f(g(a)), functions)

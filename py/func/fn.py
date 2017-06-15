import itertools


def iffun(cond, a1, a2):
    ''' call like iffun(cond, lambda: f1(), lambda: f2())'''
    return a1() if cond else a2()


### Fold

def foldl(op, initial, seq):
    return op(foldl(op, initial, seq[1:]), seq[0]) if seq else initial

def foldr(op, initial, seq):
    return op(seq[-1], foldr(op, initial, seq[:-1])) if seq else initial


### Lists

def head(L):
    return L[0] if L else None

def tail(L):
    return L[1:]

def construct(h, T):
    return [h] + T

def concat(L1, L2):
    return L1 + L2

def flatten(L):
    pass

def element_at(L, index):
    return iffun(index == 0, lambda: head(L), lambda: element_at(tail(L), index - 1))

def reverse(L):
    return concat(reverse(tail(L)), construct(head(L), [])) if L else []


def quicksort(L):
    if not L:
        return []
    h = head(L)
    T = tail(L)
    return concat(
        concat(
            quicksort(filter(lambda x: x < h, T)),
            construct(h, [])),
        quicksort(filter(lambda x: x >= h, T))
    )

### Generator-based list operations (?)

# Todo



### Currying

# Coded by Massimiliano Tomassoli, 2012.
# https://mtomassoli.wordpress.com/2012/03/18/currying-in-python/
#
# - Thanks to b49P23TIvg for suggesting that I should use a set operation
#     instead of repeated membership tests.
# - Thanks to Ian Kelly for pointing out that
#     - "minArgs = None" is better than "minArgs = -1",
#     - "if args" is better than "if len(args)", and
#     - I should use "isdisjoint".
#
def genCur(func, unique = True, minArgs = None):
    """ Generates a 'curried' version of a function. """
    def g(*myArgs, **myKwArgs):
        def f(*args, **kwArgs):
            if args or kwArgs:  # some more args!
                # Allocates data to assign to the next 'f'.
                newArgs = myArgs + args
                newKwArgs = dict.copy(myKwArgs)

                # If unique is True, we don't want repeated keyword arguments.
                if unique and not kwArgs.keys().isdisjoint(newKwArgs):
                    raise ValueError("Repeated kw arg while unique = True")

                # Adds/updates keyword arguments.
                newKwArgs.update(kwArgs)

                # Checks whether it's time to evaluate func.
                if minArgs is not None and minArgs <= len(newArgs) + len(newKwArgs):
                    return func(*newArgs, **newKwArgs)  # time to evaluate func
                else:
                    return g(*newArgs, **newKwArgs)     # returns a new 'f'
            else:                               # the evaluation was forced
                return func(*myArgs, **myKwArgs)
        return f
    return g


def cur(f, minArgs = None):
    return genCur(f, True, minArgs)


def curr(f, minArgs = None):
    return genCur(f, False, minArgs)

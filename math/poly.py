from functools import partial, reduce

import numpy as np


def polymul1(*p):
    return reduce(np.polymul, p)


def polyfrac(num, denom, val):
    numval = np.polyval(num, val)
    denomval = np.polyval(denom, val)
    quot = numval // denomval
    sign = "" if quot >= 0 else "-"
    if numval % denomval == 0:
        return f"{quot}"
    return f"{sign}({abs(numval)}/{abs(denomval)})"


def polyfracmany(num, denom, *vals):
    results = map(partial(polyfrac, num, denom), vals)
    return "\n".join(f"{val}: {rez}" for val, rez in zip(vals, results))

#!/usr/bin/env python3
import math
import numpy


def primes(limit):
    primes = numpy.arange(3, limit + 1, 2)
    isprime = numpy.ones((limit - 1)/2, dtype=bool)
    for factor in primes[:int(math.sqrt(limit))]:
        if isprime[factor - 2]:
            isprime[factor*2-2::factor] = 0
    return primes[isprime]


print(primes(1000000000000066600000000000001))

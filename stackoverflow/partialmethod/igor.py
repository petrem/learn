class Igors:
    pass


igor = Igors()
try:
    igor()  # --> raises TypeError
except TypeError:
    print("Is it deaf?")

Igors.__call__ = lambda self: print("Did you call, Marster?")

igor()  # --> prints successfully

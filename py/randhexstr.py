import random


random.seed()


def _randstr(length=6):
    return '{0:0{length}x}'.format(random.randint(0, 16**length), length=length)

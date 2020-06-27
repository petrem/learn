"""
This is exploring the consequences of a change in psycopg2's ReadDictCursor,
in 2.8 and onwards.
"""


from collections import OrderedDict


class mydict1(dict):
    def __init__(self):
        #dict.__init__(self)
        super().__init__()


class mydict2(OrderedDict):
    def __init__(self):
        super().__init__()


if __name__ == "__main__":
    d1 = mydict1()
    d2 = mydict2()

    for d in [d1, d2]:
        d["a"] = 1
        d["b"] = 2
        print(f"data: {d}")
        print(f"type: {type(d)}")
        c = d.copy()
        print(f"data: {c}")
        print(f"type: {type(c)}")

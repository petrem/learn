from decorator import decorator


@decorator
def deco1(f, *args, **kwargs):
    print(f"deco1 args: {args} kw: {kwargs}")
    return f(*args, **kwargs)


@deco1
def foo1(arg):
    print(f"foo1({arg})")


@deco1
def foo2(arg1=1, arg2=2):
    print(f"foo2({arg1}, {arg2})")


def deco_factory(*params):
    @decorator
    def deco2(f, *args, **kwargs):
        print(f"deco2 params: {params} args: {args} kw: {kwargs}")
        return f(*args, **kwargs)
    return deco2


@deco_factory(1, 2, 3)
def bar1(arg):
    print(f"bar1({arg})")


@deco_factory(4)
def bar2(arg1=1, arg2=2):
    print(f"bar2({arg1}, {arg2})")



if __name__ == "__main__":
    foo1(1)
    foo1(arg=2)
    foo2(1)
    foo2(arg2=2)
    bar1(3)
    bar1(arg=4)
    bar2(3)
    bar2(arg2=4)

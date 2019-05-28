import decorator
import re

NAMESPACES_RE = re.compile(r"^(?!pg_)[a-z][a-z0-9_]*$", re.ASCII)
ILLEGAL_NAMESPACES = ["foo", "bar"]


@decorator.decorator
def valid_namespace(fn, message="Namespace error: {}", *args, **kwargs):
    if args[0] in ILLEGAL_NAMESPACES or not re.match(NAMESPACES_RE, args[0]):
        print(message.format(args[0]))
        return None
    return fn(*args, **kwargs)


@valid_namespace()
def do_something(namespace):
    print(f"doing something in {namespace}")


@valid_namespace("Something else needs validity too: {}")
def do_something_else(namespace):
    print(f"doing something ELSE in {namespace}")


if __name__ == "__main__":
    do_something("foo")
    do_something("pg_stats")
    do_something("1shot")
    do_something("the-picard")
    do_something("finally_works")
    do_something_else("bar")
    do_something_else("finally_works")

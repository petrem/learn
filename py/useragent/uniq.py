#!/usr/bin/python3
import argparse
import collections
import shlex


def coroutine(f):
    def wrapper(*args, **kwargs):
        r = f(*args, **kwargs)
        r.__next__()
        return r
    return wrapper


@coroutine
def unique(counts, drop, take):
    try:
        while True:
            line = (yield)
            fields = shlex.split(line)
            key = fields[drop:drop+take if take else len(fields) - 1]
            counts.update(key)
    except GeneratorExit:
        pass


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file")
    parser.add_argument(
        "--drop", type=int, default=0, help="drop so many fields")
    parser.add_argument(
        "--take", type=int, default=None, help="take so many fields")
    parser.add_argument(
        "--top", type=int, default=None, help="display top N results")
    # using shlex as it's much less pain...
    # parser.add_argument('--delim', default=" ", help="field delimiter")
    args = parser.parse_args()
    counts = collections.Counter()
    uniq = unique(counts, args.drop, args.take)
    with open(args.input_file, "rt") as f:
        for line in f:
            uniq.send(line)
    uniq.close()
    for useragent, count in counts.most_common(args.top):
        print("{} {}".format(count, useragent))


if __name__ == '__main__':
    main()

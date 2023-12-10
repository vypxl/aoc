#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return [nums(l) for l in lines(inp)]


def next(seq):  # noqa
    deltas = [b - a for a, b in zip(seq[:-1], seq[1:])]
    if all(d == 0 for d in deltas):
        return seq[-1]

    return seq[-1] + next(deltas)


def p1(inp):  # noqa
    return sum(next(seq) for seq in inp)


def p2(inp):  # noqa
    return p1([list(reversed(l)) for l in inp])


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 1731106378
# Solution part 2: 1087

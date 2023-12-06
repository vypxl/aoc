#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return parts(nums(inp), 2)


def p1(inp):  # noqa
    sim = lambda a, b: sum(1 for i in range(a) if i * (a - i) > b)
    xs = [sim(a, b) for a, b in zip(*inp)]
    return prod(xs)


def p2(inp):  # noqa
    inp = [[int("".join(map(str, a)))] for a in inp]
    return p1(inp)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 800280
# Solution part 2: 45128024

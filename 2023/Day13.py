#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return [grid(x, ".#") for x in superlines(inp)]


def score(m, part):  # noqa
    part = 0 if part == 1 else 1

    def valid(i, g):
        l = min(i, g.shape[0] - i)
        g = g[i - l : i + l]
        return np.sum(g[:l] != np.flip(g[l:], axis=0)) == part

    vertical = sum(i for i in range(m.shape[1]) if valid(i, m.T))
    horizontal = sum(100 * i for i in range(m.shape[0]) if valid(i, m))

    return vertical + horizontal


def p1(inp):  # noqa
    return sum(score(g, 1) for g in inp)


def p2(inp):  # noqa
    return sum(score(g, 2) for g in inp)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 37381
# Solution part 2: 28210

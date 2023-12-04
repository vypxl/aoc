#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

# start at 17:50

from util import *


def parse_game(g):  # noqa
    gs = [x.strip().split() for x in g.split(",")]
    return [(int(i), c.strip()) for [i, c] in gs]


def parse(inp):  # noqa
    xs = []
    for l in inp.splitlines():
        g, ys = l.split(":")
        g = nums(g)[0]
        ys = [parse_game(zs) for zs in ys.split(";")]
        xs.append((g, ys))
    return xs


def p1(inp):  # noqa
    s = 0
    for g, sets in inp:
        possible = True
        for sett in sets:
            for n, col in sett:
                if (
                    (col == "red" and n > 12)
                    or (col == "green" and n > 13)
                    or (col == "blue" and n > 14)
                ):
                    possible = False
        if possible:
            s += g
    return s


def p2(inp):  # noqa
    s = 0
    for _, sets in inp:
        maxs = {
            "red": 0,
            "green": 0,
            "blue": 0,
        }
        for sett in sets:
            for n, col in sett:
                maxs[col] = max(maxs[col], n)
        p = maxs["red"] * maxs["green"] * maxs["blue"]
        s += p
    return s


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 2727
# Solution part 2: 56580

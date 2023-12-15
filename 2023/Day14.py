#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *

Empty = 0
Round = 1
Cube = 2


def parse(inp):  # noqa
    return grid(inp, ".O#")


def load(g):  # noqa
    return sum(len(g) - y for x, y in grid_indices(g) if g[y, x] == Round)


def shift(g):  # noqa
    for x, y in grid_indices(g):
        if g[y, x] != Round:
            continue
        ny = y
        while ny > 0 and g[ny - 1, x] == Empty:
            ny -= 1
        g[y, x] = Empty
        g[ny, x] = Round
    return g


def p1(g):  # noqa
    return load(shift(g.copy()))


def p2(g):  # noqa
    cycle = lambda g: applyN(lambda g: np.rot90(shift(g), 3), 4, g)
    return load(iterate_cyclic(cycle, g, 1000000000))


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 113525
# Solution part 2: 101292

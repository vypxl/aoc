#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return grid(inp, ".#")


def solve(g, expand):  # noqa
    iy = np.array(range(g.shape[0]))
    ix = np.array(range(g.shape[1]))

    for i in range(g.shape[0]):
        if g[i].sum() == 0:
            iy[i:] += expand - 1

    for j in range(g.shape[1]):
        if g[:, j].sum() == 0:
            ix[j:] += expand - 1

    galaxies = np.transpose(np.where(g == 1))
    return sum(
        abs(iy[y1] - iy[y0]) + abs(ix[x1] - ix[x0])
        for (y0, x0), (y1, x1) in it.combinations(galaxies, r=2)
    )


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{solve(inp, 2)}")
    print(f"Solution for part 2:\n{solve(inp, 1000000)}")


if __name__ == "__main__":
    main()

# Solution part 1: 9563821
# Solution part 2: 827009909817

#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


chars = "│─└┘┐┌.█XO"

empty = 6
start = 7
inside = 8
outside = 9
turns_l = [3, 5]
turns_r = [2, 4]
dname = {(-1, 0): "up", (1, 0): "down", (0, 1): "right", (0, -1): "left"}


def parse(inp):  # noqa
    return grid(inp, "|-LJ7F.S")


def valid(g):  # noqa
    def _v(a, b):
        y0, x0 = a
        y1, x1 = b
        src = g[y0, x0]
        dest = g[y1, x1]
        dir = (y1 - y0, x1 - x0)

        t = True
        f = False

        lookup = {
            (1, 0): ([t, f, f, f, t, t, f, t], [t, f, t, t, f, f, f, t]),
            (-1, 0): ([t, f, t, t, f, f, f, t], [t, f, f, f, t, t, f, t]),
            (0, 1): ([f, t, t, f, f, t, f, t], [f, t, f, t, t, f, f, t]),
            (0, -1): ([f, t, f, t, t, f, f, t], [f, t, t, f, f, t, f, t]),
        }[dir]

        return lookup[0][src] and lookup[1][dest]

    return _v


def p1(g):  # noqa
    s = np.where(g == start)
    s = (s[0][0], s[1][0])
    dist, _ = dijkstra_grid(g, s, neighbours_straight, valid(g))
    return max(dict(dist).values())


def p2(g):  # noqa
    s = np.where(g == start)
    s = (s[0][0], s[1][0])
    _, prev = dijkstra_grid(g, s, neighbours_straight, valid(g))

    # purge all unnecessary pipe fragments
    for i in range(g.shape[0]):
        for j in range(g.shape[1]):
            if (i, j) not in prev or g[i, j] == empty:
                g[i, j] = empty

    # reinstate start tile
    g[s] = start

    # count the number of line crossings of each empty tile
    # if it's even, the tile is outside the loop, otherwise it's inside.
    for i in range(g.shape[0]):
        for j in range(g.shape[1]):
            if g[i, j] != empty:
                continue

            crossings = sum(1 for x in range(j, g.shape[1]) if g[i, x] in [0, 2, 3])
            g[i, j] = [outside, inside][crossings % 2]

    return len(np.where(g == inside)[0])


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 6831
# Solution part 2: 305

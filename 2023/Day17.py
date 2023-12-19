#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return grid(inp, "0123456789")


def solve(g, step_min=0, step_max=np.inf):  # noqa
    def neighs(node):
        (x, y), last_dir, timeout = node
        xs = []
        for dx, dy in neighbours_straight:
            dir = (dx, dy)
            nx, ny = x + dx, y + dy

            if not grid_index_valid(g, ny, nx):
                continue

            if dir[0] == -last_dir[0] and dir[1] == -last_dir[1]:
                continue

            if dir == last_dir:
                if timeout >= step_max:
                    continue
                xs.append(((nx, ny), dir, timeout + 1))
            else:
                if timeout < step_min:
                    continue
                xs.append(((nx, ny), dir, 1))

        return [(x, g[x[0][1], x[0][0]]) for x in xs]

    goal = g.shape[1] - 1, g.shape[0] - 1
    dist, _ = dijkstra(((0, 0), (0, 0), step_min), neighs)

    return min(dist[x] for x in dist if x[0] == goal and x[2] >= step_min)


def p1(inp):  # noqa
    return solve(inp, 0, 3)


def p2(inp):  # noqa
    return solve(inp, 4, 10)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 886
# Solution part 2: 1055

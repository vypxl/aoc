#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *

empty, wall, start = 0, 1, 2


def parse(inp):  # noqa
    return grid(inp, ".#S")


def get_dist(inp):  # noqa
    S = np.where(inp == start)
    S = (S[0][0], S[1][0])
    inp[*S] = empty
    dist, _ = bfs_grid(S, inp, neighbours_straight, lambda _, o: inp[o] == empty)

    inp[*S] = start

    return dist


def p1(inp):  # noqa
    dist = get_dist(inp)
    return sum(1 for d in dist.values() if d <= 64 and (d - 64) % 2 == 0)


def p2(inp):  # noqa
    dist = np.array(list(get_dist(inp).values()))
    even, odd = dist[dist % 2 == 0], dist[dist % 2 == 1]
    corner_even, corner_odd = even[even > 65], odd[odd > 65]

    n = 202300
    return (
        ((n + 1) * (n + 1)) * len(odd)
        + (n * n) * len(even)
        - (n + 1) * len(corner_odd)
        + n * len(corner_even)
    )


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 3743
# Solution part 2: 618261433219147

#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    directions, nodes = inp.split("\n\n")

    directions = ["LR".index(x) for x in directions]

    nodes = re.findall(r"[A-Z]{3}", nodes)
    nodes = chunks(nodes, 3)  # chunks divides a list into chunks of length k

    return directions, dict((a[0], (a[1], a[2])) for a in nodes)


def solve(inp):  # noqa
    directions, nodes = inp
    positions = [k for k in nodes.keys() if k.endswith("A")]
    aaa_index = positions.index("AAA")

    step_counts = [-1 for _ in range(len(positions))]
    i = 0

    while any(not x >= 0 for x in step_counts):
        dir = directions[i % len(directions)]
        positions = [nodes[p][dir] for p in positions]

        for j, p in enumerate(positions):
            if p.endswith("Z") and step_counts[j] < 0:
                step_counts[j] = i + 1

        i += 1

    p1 = step_counts[aaa_index]
    p2 = math.lcm(*step_counts)
    return p1, p2


def main():  # noqa
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")


if __name__ == "__main__":
    main()

# solution part 1: 11309
# Solution part 2: 13740108158591
# Leaderboard: 815 / 1441

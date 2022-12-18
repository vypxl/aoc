#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

neighbours = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

def parse(inp):
    xs = nums(inp)
    mx = max(xs) + 3
    g = np.zeros((mx,mx,mx), dtype=int)
    cubes = [tuple(x) for x in chunks(xs, 3)]
    g[tuple(zip(*cubes))] = 1

    cube_neigbours_f = lambda p: [(p[0] + dx, p[1] + dy, p[2] + dz) for dx, dy, dz in neighbours if (p[0] + dx >= 0 and p[0] + dx < mx and p[1] + dy >= 0 and p[1] + dy < mx and p[2] + dz >= 0 and p[2] + dz < mx and g[p[0] + dx, p[1] + dy, p[2] + dz] != 1)]

    reachable_from_edge = reach((0, 0, 0), cube_neigbours_f)
    g[tuple(zip(*reachable_from_edge))] = 2
    return g, cubes

def p1(inp):
    g, cubes = inp

    return sum(len([0 for dx, dy, dz in neighbours if g[x+dx, y+dy, z+dz] != 1]) for x, y, z in cubes)

def p2(inp):
    g, cubes = inp

    return sum(len([0 for dx, dy, dz in neighbours if g[x+dx, y+dy, z+dz] == 2]) for x, y, z in cubes)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 4460
# Solution part 2: 2498

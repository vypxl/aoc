#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [np.array(nums(g)[1:]).reshape((-1, 3)).tolist() for g in superlines(inp)]

def rotates(v):
    x, y, z = v
    return [(x, z, -y), (-z, x, -y), (-x, -z, -y), (z, -x, -y), (z, -y, x), (y, z, x), (-z, y, x), (-y, -z, x), (-y, x, z), (-x, -y, z), (y, -x, z), (x, y, z), (-z, -x, y), (x, -z, y), (z, x, y), (-x, z, y), (-x, y, -z), (-y, -x, -z), (x, -y, -z), (y, x, -z), (y, -z, -x), (z, y, -x), (-y, z, -x), (-z, -y, -x)]

def rotate(v, i):
    if not isinstance(i, list): i = [i]
    for r in i:
        v = rotates(v)[r]
    return v

def overlap(b1, b2):
    offsets = defaultdict(lambda: defaultdict(lambda: 0))
    for v in b1:
        for u in b2:
            for i, ur in enumerate(rotates(u)):
                off = v[0] - ur[0], v[1] - ur[1], v[2] - ur[2]
                offsets[i][off] += 1
                if offsets[i][off] >= 12:
                    return i, off

    return None

def union(b1, b2, rot, off):
    s = set()
    for v in b1:
        s.add(tuple(v))
    for v in b2:
        x, y, z = rotate(v, rot)
        dx, dy, dz = off
        s.add((x + dx, y + dy, z + dz))

    return list(s)

def dist(u, v = (0, 0, 0)):
    return abs(u[0] - v[0]) + abs(u[1] - v[1]) + abs(u[2] - v[2])

def ls(x):
    if isinstance(x, list): return x
    return [x]

# "it works (TM)"
def solve(inp):
    # Compute all overlaps
    rel = [[None for _ in range(len(inp))] for _ in range(len(inp))]
    for i, j in it.product(range(len(inp)), repeat=2):
        a, b = inp[i], inp[j]
        r = overlap(a, b)
        if r is not None:
            rot, off = r
            rel[i][j] = (rot, off)

    # Compute actual number of beacons and scanner positions
    pos = {0:(0, 0, 0)}
    while len(set(map(len, inp))) > 1:
        for i in range(len(inp)):
            for j in range(len(inp)):
                ro = rel[i][j]
                if ro is not None and ro[1] is not None:
                    rot, off = ro

                    # calculate position relative to scanner 0
                    if i in pos and j not in pos:
                        x, y, z = pos[i]
                        myrot = rel[0][i][0]
                        dx, dy, dz = rotate(off, myrot)

                        pos[j] = (x + dx, y + dy, z + dz)

                        if (rel[0][j] is None): rel[0][j] = ([rot] + ls(rel[0][i][0]), None)

                    # merge beacon positions
                    inp[i] = union(inp[i], inp[j], rot, off)

    nbeacons = len(inp[-1])
    mdist = max([dist(pos[i], pos[j]) for i in range(len(inp)) for j in range(len(inp))])
    return nbeacons, mdist

def main():
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 438
# Solution part 2: 11985

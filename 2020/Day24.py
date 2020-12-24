#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

offsets = {
    'e': np.array([ 1,  0, -1]),
    'w': np.array([-1,  0,  1]),
    'a': np.array([ 0,  1, -1]),
    'b': np.array([-1,  1,  0]),
    'c': np.array([ 0, -1,  1]),
    'd': np.array([ 1, -1,  0])
}

WHITE, BLACK = False, True

def neighbours(coord):
    x, y, z = coord
    return [(x + a[0], y + a[1], z + a[2]) for a in offsets.values()]

def parse(inp):
    tiles = {}
    for l in inp:
        s = l.replace('se', 'a').replace('sw', 'b').replace('nw', 'c').replace('ne', 'd')
        coord = tuple(sum(offsets[x] for x in s))
        if coord in tiles:
            tiles[coord] = not tiles[coord]
        else:
            tiles[coord] = BLACK

    return tiles

def p1(inp):
    return count(filter(None, inp.values()))

def p2(tiles):
    for i in range(100):
        toflip = []
        toconsider = set()
        for coord in tiles:
            toconsider.add(coord)
            toconsider |= set(neighbours(coord))

        for coord in toconsider:
            if coord not in tiles:
                tiles[coord] = WHITE
            neighs = count(filter(None, [tiles.get(n, WHITE) for n in neighbours(coord)]))

            if (tiles[coord] is BLACK and (neighs == 0 or neighs > 2)) or (tiles[coord] is WHITE and neighs == 2):
                toflip.append(coord)

        for coord in toflip:
            tiles[coord] = not tiles[coord]

    return p1(tiles)

def main():
    inp = parse(data_lines())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 326
# Solution part 2: 3979

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *
import random

VISUALIZE = False
SRC = (0, 500)
AIR, ROCK, MOVING_SAND, RESTING_SAND, FLOOR = [0, 1, 2, 3, 4]

def parse(inp):
    paths = [chunks(nums(i), 2) for i in inp.splitlines()]
    mx = max(map(fst, flatten(paths))) * 2
    my = max(map(snd, flatten(paths))) + 2
    g = np.zeros((mx+1, my+1), dtype=int)

    for moves in paths:
        px, py = moves[0]
        for x, y in moves[1:]:
            g[min(px, x):max(px,x)+1, min(py, y):max(py, y)+1] = ROCK

            px = x
            py = y

    return g.transpose(), my

def step(g, floor, p):
    y, x = p

    moves = [(y+1, x), (y+1, x-1), (y+1, x+1)]
    for ny, nx in moves:
        if ny >= floor:
            # The current move would come to rest on the floor
            g[y, x] = RESTING_SAND
            g[ny, nx] = FLOOR
            return None

        if g[ny, nx] != AIR:
            # The current move is illegal
            continue

        g[y, x] = AIR
        g[ny, nx] = MOVING_SAND
        return (ny, nx)

    # No move possible
    g[y, x] = RESTING_SAND
    return None

def sand(g, floor):
    p = SRC
    while True:
        np = step(g, floor, p)
        if np:
            p = np # yes
        else:
            if VISUALIZE and random.randint(0, 100) < 5:
                printgrid(g, " █░▒▀", True)
            return p

def solve(inp):
    g, floor = inp
    c = 0
    sol_part1 = None
    while True:
        rest_pos = sand(g, floor)

        if not sol_part1 and rest_pos[0] >= floor-1:
            sol_part1 = c

        c += 1
        if rest_pos == SRC:
            break

    return sol_part1, c

def main():
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 1133
# Solution part 2: 27566

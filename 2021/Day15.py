#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def p1(inp):
    return dijkstra_grid(inp, (0, 0), neighbours_straight, inp)[0][(inp.shape[0]-1, inp.shape[1]-1)]

def p2(inp):
    w, h = inp.shape
    grid = np.zeros((w*5, h*5), dtype=int)

    for i in range(5):
        for j in range(5):
            grid[i*w:(i+1)*w, j*h:(j+1)*h] = np.mod(inp - 1 + i + j, 9) + 1

    return p1(grid)

def main():
    inp = grid(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 687
# Solution part 2: 2957
# Leaderboard: 867 / 294

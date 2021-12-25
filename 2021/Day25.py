#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return grid(inp, '.>v')

def p1(inp):
    grid = inp
    w, h = grid.shape
    idx = 0
    while True:
        idx += 1
        grid2 = np.zeros(grid.shape, dtype=int)
        for i, j in np.argwhere(grid == 1):
            if grid[i, (j+1) % h] == 0: grid2[i, (j+1) % h] = 1
            else: grid2[i, j] = 1
        
        grid3 = np.copy(grid2)
        grid2[grid==2] = 2
        for i, j in np.argwhere(grid == 2):
            if grid2[(i+1) % w, j] == 0: grid3[(i+1) % w, j] = 2
            else: grid3[i, j] = 2
        
        if np.array_equal(grid, grid3): return idx
        grid = grid3

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 598

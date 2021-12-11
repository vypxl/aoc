#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def increase(grid, i, j, in_flash=False):
    if not grid_index_valid(grid, i, j): return
    if in_flash and grid[i, j] == 0: return
    grid[i, j] += 1

    if grid[i, j] > 9:
        grid[i, j] = 0
        return True
    return False

def step(grid):
    flash = []

    for i, j in grid_indices(grid):
        if increase(grid, i, j):
            flash.append((i, j))

    count = 0

    while len(flash) > 0:
        count += 1
        i, j = flash.pop(0)
        for x, y in neighbours_both:
            if increase(grid, i + x, j + y, True):
                flash.append((i + x, j + y))

    return count

def p1(inp):
    return sum(step(inp) for _ in range(100))

def p2(inp):
    i = 0
    while True:
        i += 1
        if step(inp) == inp.size:
            return i

def main():
    inp = grid(data())
    print(f"Solution for part 1:\n{p1(inp.copy())}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1735
# Solution part 2: 400

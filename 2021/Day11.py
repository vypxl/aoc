#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def increase(grid, i, j, in_flash=False):
    if i < 0 or j < 0 or i >= grid.shape[0] or j >= grid.shape[1]: return
    if in_flash and grid[i, j] == 0: return
    grid[i, j] += 1

    if grid[i, j] > 9:
        grid[i, j] = 0
        return (i, j)
    return None

neighbours = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

def step(grid):
    flash = []

    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            a = increase(grid, i, j)
            if a is not None:
                flash.append(a)

    count = 0

    while len(flash) > 0:
        count += 1
        i, j = flash.pop(0)
        for x, y in neighbours:
            a = increase(grid, i + x, j + y, True)
            if a is not None:
                flash.append(a)

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

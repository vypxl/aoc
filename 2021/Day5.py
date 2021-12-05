#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return np.array(inp), np.max(inp) + 1

def p1(inp):
    lines, dim = inp
    grid = np.zeros((dim, dim))

    for line in lines:
        x1, y1, x2, y2 = line

        if not (x1 == x2 or y1 == y2):
            continue

        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                grid[x, y] += 1

    return np.sum(grid >= 2)

def p2(inp):
    lines, dim = inp
    grid = np.zeros((dim, dim))

    for line in lines:
        x1, y1, x2, y2 = line

        if not (x1 == x2 or y1 == y2):
            dx = 1 if x2 > x1 else -1
            dy = 1 if y2 > y1 else -1

            for i in range(abs(x1 - x2) + 1):
                grid[x1 + dx * i, y1 + dy * i] += 1
        else:
            for x in range(min(x1, x2), max(x1, x2) + 1):
                for y in range(min(y1, y2), max(y1, y2) + 1):
                    grid[x, y] += 1

    return np.sum(grid >= 2)

def main():
    inp = parse(data_lines_nums())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 5576
# Solution part 2: 18144

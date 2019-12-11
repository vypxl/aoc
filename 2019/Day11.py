#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import data
import numpy as np
from intcode import IntComputer

PRINT_ASM = False
GRIDSIZE = 512
GRIDMID = GRIDSIZE // 2

def bothParts(inp, debug=False):
    grid = np.zeros((GRIDSIZE, GRIDSIZE), dtype=np.int8)
    rX, rY, rA = GRIDMID, GRIDMID, 0
    readingColor = True

    seen = set()

    def read():
        nonlocal grid, rX, rY
        return grid[rX, rY]

    def write(val):
        nonlocal grid, rX, rY, rA, readingColor
        if readingColor:
            grid[rX, rY] = val
            seen.add((rX, rY))
        else:
            if val == 0:
                rA = np.mod(rA - np.pi / 2, np.pi * 2)
            else:
                rA = np.mod(rA + np.pi / 2, np.pi * 2)
            rX += int(np.sin(rA))
            rY += int(np.cos(rA))
        readingColor = not readingColor

    robot = IntComputer(inp, name="Day 11", readf=read, writef=write, debug=debug)

    robot.run()
    p1 = len(seen)

    ### Part 2

    grid.fill(0)
    grid[GRIDMID, GRIDMID] = 1
    rX, rY, rA = GRIDMID, GRIDMID, (-np.pi / 2)
    readingColor = True

    robot.run()

    coords = np.argwhere(grid)
    mx, my = coords.min(axis=0)
    Mx, My = coords.max(axis=0)
    img = grid[mx : Mx + 1, my : My + 1]

    return (
        p1,
        "\n".join(map(lambda row: "".join(map(str, row)), img))
        .replace("0", " ")
        .replace("1", "█"),
    )


def main():
    inp = data()

    p1result, p2result = bothParts(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")


if __name__ == "__main__":
    main()

# Solution part 1: 1564
# Solution part 2: RFEPCFEB

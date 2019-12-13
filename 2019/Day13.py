#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
import sys
import numpy as np
import util
from intcode import IntComputer

PRINT_ASM = False
DISPLAY = True
GRIDSIZE = (44, 22)
MPX = 0
MPY = 1
MTI = 2
MMX = MTI + 1
COLORS = [(255, 255, 255), (0, 0, 0), (127, 127, 127), (12, 220, 34), (180, 52, 8)]


def p1(inp, debug=False):
    grid = np.zeros(GRIDSIZE, dtype=np.int8)
    mode = MPX
    data = np.array([0, 0, 0])

    def write(val):
        nonlocal grid, mode
        data[mode] = val

        if mode == MTI:
            grid[data[MPX], data[MPY]] = data[MTI]

        mode = (mode + 1) % MMX

    robot = IntComputer(inp, name="Day 13", writef=write, debug=debug)

    robot.run()

    return grid[grid == 2].size


def p2(inp, debug=False):
    if DISPLAY:
        import pygame as pg
    mode = MPX
    data = np.array([0, 0, 0])

    if DISPLAY:
        pg.init()
        screen = pg.display.set_mode((GRIDSIZE[0] * 16, GRIDSIZE[1] * 16))
        grid = pg.surface.Surface(GRIDSIZE)

    ballpos = 0
    padpos = 0
    score = 0

    def display():
        pg.transform.scale(grid, (GRIDSIZE[0] * 16, GRIDSIZE[1] * 16), screen)
        pg.display.flip()

    def events():
        for ev in pg.event.get():
            if ev.type == pg.QUIT:
                sys.exit(0)

    def read():
        nonlocal display, events, ballpos, padpos
        if DISPLAY:
            events()
            display()
        return np.sign(padpos - ballpos)

    def write(val):
        nonlocal mode, data, grid, ballpos, padpos, score
        data[mode] = val

        if mode == MTI:
            if data[MPX] == -1 and data[MPY] == 0:
                score = data[MTI]
            else:
                if DISPLAY:
                    pg.draw.circle(grid, COLORS[data[MTI]], (data[MPX], data[MPY]), 0)
                if data[MTI] == 3:
                    ballpos = data[MPX]
                elif data[MTI] == 4:
                    padpos = data[MPX]

        mode = (mode + 1) % MMX

    robot = IntComputer(
        "2" + inp[1:], name="Day 13", readf=read, writef=write, debug=debug
    )
    robot.run()

    return score


def main():
    inp = util.data()

    p1result = p1(inp, debug=PRINT_ASM)
    p2result = p2(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")


if __name__ == "__main__":
    main()

# Solution part 1: 326
# Solution part 2: 15988

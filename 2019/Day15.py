#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
import sys
import numpy as np
from util import data
from intcode import IntComputer

PRINT_ASM = False
DISPLAY = True

GRIDSIZE = (128, 128)
GRIDMID = (GRIDSIZE[0] // 2, GRIDSIZE[1] // 2)
SCALE = 8

UNKNOWN = 0x87CEEB
WALL = 0x333333
FREE = 0xFFFFFF
OXYGEN = 0x21E45E
SEARCHED = 0x43C829
PATH = 0xC4211B
TILE_LOOKUP = [WALL, FREE, OXYGEN]
DIRECTIONS = [
    (1, np.array([0, 1])),
    (4, np.array([1, 0])),
    (2, np.array([0, -1])),
    (3, np.array([-1, 0])),
]

MODE_LEN = 0
MODE_MAX = 1


def dijkstra(grid, src, mode, display):
    oldgrid = grid.copy()
    Q = set()
    dist, prev = dict(), dict()
    for x in range(grid.shape[0]):
        for y in range(grid.shape[1]):
            pos = (x, y)
            if grid[pos] == UNKNOWN or grid[pos] == WALL:
                continue
            dist[pos] = np.inf
            prev[pos] = None
            Q.add(pos)
    dist[src] = 0

    while Q:
        u = min(Q, key=lambda x: dist[x])
        ux, uy = u
        Q.remove(u)
        grid[u] = SEARCHED
        if DISPLAY:
            display()
        for v in [(ux + 1, uy), (ux - 1, uy), (ux, uy + 1), (ux, uy - 1)]:
            if grid[v] == WALL:
                continue
            if grid[v] == UNKNOWN:
                grid[:] = oldgrid
                return -1
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u

    dest = tuple(np.array(np.where(oldgrid == OXYGEN))[:, 0])

    if DISPLAY:
        S = []
        u = dest
        while 1:
            S.insert(0, u)
            u = prev[u]
            if not u:
                break
            grid[u] = PATH
            display()

    grid[:] = oldgrid

    if mode == MODE_LEN:
        return dist[dest]
    elif mode == MODE_MAX:
        return max(dist.values())


def bothParts(inp, debug=False):
    grid = np.full(GRIDSIZE, UNKNOWN)
    grid[GRIDMID] = FREE
    pos = np.array(GRIDMID)
    cur_move = 0
    last_turn = 0
    ret, ret2 = -1, -1

    if DISPLAY:
        import pygame as pg

        pg.init()
        screen = pg.display.set_mode((GRIDSIZE[0] * SCALE, GRIDSIZE[1] * SCALE))
        gridsurf = pg.surface.Surface(GRIDSIZE)

    def display():
        for ev in pg.event.get():
            if ev.type == pg.QUIT:
                sys.exit(0)
        pg.surfarray.blit_array(gridsurf, grid)
        pg.draw.circle(gridsurf, 0xE8214C, tuple(pos), 0)
        pg.transform.scale(gridsurf, (GRIDSIZE[0] * SCALE, GRIDSIZE[1] * SCALE), screen)
        pg.display.flip()

    def read():
        nonlocal cur_move, pos, last_turn
        move = cur_move

        ORDER = [-1, 0, 1]

        for o in ORDER:
            _move = (move + o) % len(DIRECTIONS)
            last_turn = o
            npos = pos + DIRECTIONS[_move][1]
            cell = grid[npos[0], npos[1]]
            if cell != WALL:
                break

        cur_move = _move
        return DIRECTIONS[cur_move][0]

    def write(val):
        nonlocal grid, pos, cur_move, ret, ret2
        npos = pos + DIRECTIONS[cur_move][1]
        if val == 0:
            if grid[npos[0], npos[1]] == UNKNOWN:
                cur_move = (cur_move - last_turn) % len(DIRECTIONS)
            grid[npos[0], npos[1]] = WALL
        elif val == 1:
            grid[npos[0], npos[1]] = FREE
            pos = npos
        elif val == 2:
            grid[npos[0], npos[1]] = OXYGEN
            pos = npos
            ret = dijkstra(grid, GRIDMID, MODE_LEN, display)
            ret2 = dijkstra(
                grid, tuple(np.array(np.where(grid == OXYGEN))[:, 0]), MODE_MAX, display
            )
            if ret != -1:
                return IntComputer.ABORT

        if DISPLAY:
            display()

    droid = IntComputer(inp, readf=read, name="Droid", writef=write, debug=debug)
    droid.run()

    return ret, ret2


def main():
    inp = data()

    p1result, p2result = bothParts(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")


if __name__ == "__main__":
    main()

# Solution part 1: 208
# Solution part 2: 306

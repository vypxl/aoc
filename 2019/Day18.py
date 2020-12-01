#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
import re
import numpy as np
from util import data, lmap

PRINT_ASM = False
DISPLAY = True

SCALE = 8

WALL = 0x333333
FREE = 0xFFFFFF
KEY = 0x21E45E
DOOR = 0x5E421E4
SEARCHED = 0x43C829
PATH = 0xC4211B
POS = 0xE8214C
DIRECTIONS = [
    np.array([0, 1]),
    np.array([1, 0]),
    np.array([0, -1]),
    np.array([-1, 0]),
]


def dijkstra(grid, allkeys, src):
    oldgrid = grid.copy()
    Q = set()
    dist, prev = dict(), dict()
    for x in range(grid.shape[0]):
        for y in range(grid.shape[1]):
            pos = (x, y)
            if grid[pos] == '#':
                continue
            dist[pos] = np.inf
            prev[pos] = None
            Q.add(pos)
    dist[src] = 0

    keys = set()
    keydist = dict()

    while Q:
        u = min(Q, key=lambda x: dist[x])
        ux, uy = u
        Q.remove(u)
        # grid[u] = SEARCHED
        # if DISPLAY:
        #     display()
        for v in [(ux + 1, uy), (ux - 1, uy), (ux, uy + 1), (ux, uy - 1)]:
            if grid[v] == '#':
                continue
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u
                if re.match(r'[a-z]', grid[v]):
                    keys.add(grid[v])
                    keydist[grid[v]] = dist[v]
            

    # dest = tuple(np.array(np.where(oldgrid == OXYGEN))[:, 0])

    # if DISPLAY:
    #     S = []
    #     u = dest
    #     while 1:
    #         S.insert(0, u)
    #         u = prev[u]
    #         if not u:
    #             break
    #         grid[u] = PATH
    #         display()

    # grid[:] = oldgrid

    # return dist[dest]


def p1(inp):
    grid = np.array(lmap(list, inp.strip().splitlines()))
    pos = np.array(np.where(grid == "@"))[:, 0]
    grid[tuple(pos)] = "."
    allkeys = set(list(re.sub(r"[^a-z]", "", inp)))
    # print(allkeys)

    # if DISPLAY:
    #     import pygame as pg

    #     pg.init()
    #     screen = pg.display.set_mode((grid.shape[0] * SCALE, grid.shape[1] * SCALE))
    #     gridsurf = pg.surface.Surface(grid.shape)

    # def display():
    #     for ev in pg.event.get():
    #         if ev.type == pg.QUIT:
    #             sys.exit(0)
    #     pg.surfarray.blit_array(gridsurf, grid)
    #     pg.draw.circle(gridsurf, POS, tuple(pos), 0)
    #     pg.transform.scale(
    #         gridsurf, (grid.shape[0] * SCALE, grid.shape[1] * SCALE), screen
    #     )
    #     pg.display.flip()

    return dijkstra(grid, allkeys, tuple(pos))


def p2(inp):
    return "Not Implemented"


def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1:
# Solution part 2:

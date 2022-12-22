#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

TURN_LEFT = 'L'
TURN_RIGHT = 'R'

VOID = 0
FREE = 1
WALL = 2
VISITED = 3

DOWN = (1, 0)
UP = (-1, 0)
RIGHT = (0, 1)
LEFT = (0, -1)

ROT_R = {
    (0, 1): (1, 0),
    (1, 0): (0, -1),
    (0, -1): (-1, 0),
    (-1, 0): (0, 1),
}

ROT_L = {
    (0, 1): (-1, 0),
    (-1, 0): (0, -1),
    (0, -1): (1, 0),
    (1, 0): (0, 1),
}

OPPOSITE = {
    (0, 1): (0, -1),
    (0, -1): (0, 1),
    (1, 0): (-1, 0),
    (-1, 0): (1, 0),
}

FACING_ID = {
    (0, 1): 0,
    (1, 0): 1,
    (0, -1): 2,
    (-1, 0): 3,
}

def parse(inp):
    map, moves = superlines(inp)
    moves = [int(x) if x.isnumeric() else x for x in re.findall(r'\d+|[RL]', moves)]

    ls = lines(map)
    mx = max([len(x) for x in ls])
    for i in range(len(ls)):
        ls[i] = ls[i].ljust(mx, ' ')
    map = '\n'.join(ls)
    _g = grid(map, " .#")
    g = np.zeros((_g.shape[0] + 2, _g.shape[1] + 2), dtype=int)
    g[1:-1, 1:-1] = _g
    start = (1, g[1].tolist().index(1))

    return g, moves, start

def wrap_simple(g, dir, p):
    nd = OPPOSITE[dir]
    while True:
        np = (p[0] + nd[0], p[1] + nd[1])
        if g[np] == VOID:
            return p, dir
        p = np


def wrap_cube(g, dir, p):
    w = (min(g.shape) - 2) // 3
    y, x = p
    dx, dy = (x-1) % w, (y-1) % w
    dyi = (w-dy-1)
    a0, ae = 1, w
    b0, be = 1*w+1, 2*w
    c0, ce = 2*w+1, 3*w
    d0, de = 3*w+1, 4*w
    a = range(a0, ae+1)
    b = range(b0, be+1)
    c = range(c0, ce+1)
    d = range(d0, de+1)

    # 1
    if x == b0-1 and y in a and dir == LEFT:
        return (c0 + dyi, a0), RIGHT
    # 2
    if x in b and y == a0-1 and dir == UP:
        return (d0 + dx, a0), RIGHT
    # 3
    if x in c and y == a0-1 and dir == UP:
        return (de, a0 + dx), UP
    # 4
    if x == ce+1 and y in a and dir == RIGHT:
        return (c0 + dyi, be), LEFT
    # 5
    if x in c and y == ae+1 and dir == DOWN:
        return (b0 + dx, be), LEFT
    # 6
    if x == be+1 and y in b and dir == RIGHT:
        return (ae, c0 + dy), UP
    # 7
    if x == b0-1 and y in b and dir == LEFT:
        return (c0, a0 + dy), DOWN
    # 8
    if x in a and y == c0-1 and dir == UP:
        return (b0 + dx, b0), RIGHT
    # 9
    if x == a0-1 and y in c and dir == LEFT:
        return (a0 + dyi, b0), RIGHT
    # 10
    if x == a0-1 and y in d and dir == LEFT:
        return (a0, b0 + dy), DOWN
    # 11
    if x in a and y == de+1 and dir == DOWN:
        return (a0, c0 + dx), DOWN
    # 12
    if x == ae+1 and y in d and dir == RIGHT:
        return (ce, b0 + dy), UP
    # 13
    if x in b and y == ce+1 and dir == DOWN:
        return (d0 + dx, ae), LEFT
    # 14
    if x == be+1 and y in c and dir == RIGHT:
        return (a0 + dyi, ce), LEFT

    raise Exception("Case not handled")

def solve(inp, wrap):
    g, moves, p = inp
    dir = (0, 1)
    for move in moves:
        if move == TURN_LEFT:
            dir = ROT_L[dir]
        elif move == TURN_RIGHT:
            dir = ROT_R[dir]
        else:
            for _ in range(move):
                np = (p[0] + dir[0], p[1] + dir[1])
                ndir = dir
                if g[np] == VOID:
                    np, ndir = wrap(g, dir, np)
                if g[np] == WALL:
                    break
                else:
                    dir = ndir
                    g[p] = VISITED
                    p = np

    return p[0] * 1000 + p[1] * 4 + FACING_ID[dir]

def p1(inp):
    return solve(inp, wrap_simple)

def p2(inp):
    return solve(inp, wrap_cube)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 36518
# Solution part 2: 143208

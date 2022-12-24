#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

UP = 0
DOWN = 1
LEFT = 2
RIGHT = 3

def parse(inp):
    g = grid(inp, "^v<>.#")

    blizz = [np.zeros((g.shape[0] - 2, g.shape[1] - 2)) for _ in range(4)]
    blizz[UP] = g[1:-1, 1:-1] == UP
    blizz[DOWN] = g[1:-1, 1:-1] == DOWN
    blizz[LEFT] = g[1:-1, 1:-1] == LEFT
    blizz[RIGHT] = g[1:-1, 1:-1] == RIGHT

    h, w = blizz[UP].shape

    wall = np.ones((h + 4, w + 4), dtype=bool)
    wall[2:-2, 2:-2] = False
    wall[1, 2] = False
    wall[h+2, w+1] = False

    blizzes = []
    for _ in range(h * w):
        wrap = np.copy(wall)
        wrap[2:-2, 2:-2] = blizz[UP] | blizz[DOWN] | blizz[LEFT] | blizz[RIGHT]
        blizzes.append(wrap)
        blizz = moveblizz(blizz)

    return blizzes, w*h, (2, 1), (w+1, h+2)

def moveblizz(blizz):
    return [
        np.roll(blizz[UP], -1, axis=0),
        np.roll(blizz[DOWN], 1, axis=0),
        np.roll(blizz[LEFT], -1, axis=1),
        np.roll(blizz[RIGHT], 1, axis=1),
    ]

def neighbours(blizzes, mod, p2=False, D=None):
    def n(pos):
        if p2:
            step, phase, xy = pos
        else:
            step, xy = pos
        x, y = xy
        ns = [
            (x, y),
            (x + 1, y),
            (x - 1, y),
            (x, y + 1),
            (x, y - 1),
        ]
        nstep = (step + 1) % mod


        if p2:
            if xy == D and phase == 0:
                return [((nstep, 1, D), 1)]
            elif xy == (2, 1) and phase == 1:
                return [((nstep, 2, (2, 1)), 1)]

            return [((nstep, phase, (nx, ny)), 1) for (nx, ny) in ns if not blizzes[nstep][ny, nx]]
        else:
            return [((nstep, (nx, ny)), 1) for (nx, ny) in ns if not blizzes[nstep][ny, nx]]

    return n

def p1(inp):
    blizzes, mod, S, D = inp

    return shortest_path((0, S), neighbours(blizzes, mod), lambda v: v[1] == D)

def p2(inp):
    blizzes, mod, S, D = inp

    return shortest_path((0, 0, S), neighbours(blizzes, mod, True, D), lambda v: v[1] == 2 and v[2] == D)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 274
# Solution part 2: 839
# Leaderboard: 791 / 1160

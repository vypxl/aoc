#! /usr/bin/env python
# pylint: disable=unused-wildcard-import

from util import *
from queue import PriorityQueue
from collections import defaultdict

def p1(inp):
    S = (0, 0)
    Q = PriorityQueue()
    Q.put((0, S))
    seen = set()
    d = defaultdict(lambda: np.inf)
    d[S] = 0

    while not Q.empty():
        el = Q.get()
        x, y = el[1]

        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) in seen or not grid_index_valid(inp, nx, ny):
                continue
            seen.add((nx, ny))
            nd = d[(x, y)] + inp[nx, ny]
            if nd < d[(nx, ny)]: d[(nx, ny)] = nd
            Q.put((d[(nx, ny)], (nx, ny)))

    return d[(inp.shape[0] - 1, inp.shape[1] - 1)]

def p2(inp):
    grid = np.zeros((inp.shape[0]*5, inp.shape[1]*5), dtype=int)
    inp = inp - 1
    for i in range(5):
        for j in range(5):
            grid[i*inp.shape[0]:(i+1)*inp.shape[0], j*inp.shape[1]:(j+1)*inp.shape[1]] = np.mod(inp + i + j, 9) + 1

    return p1(grid)

def main():
    inp = grid(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 687
# Solution part 2: 2957
# Leaderboard: 867 / 294

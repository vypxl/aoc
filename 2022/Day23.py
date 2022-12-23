#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    g = grid(inp, '.#')
    sz = 150
    actual = np.zeros((sz * 2, sz * 2), dtype=np.int8)
    actual[sz:sz+g.shape[0], sz:sz+g.shape[1]] = g
    return actual

def step(g, round):
    prop = np.zeros_like(g, dtype=int)
    ng = np.zeros_like(g, dtype=int)

    cond = [
        lambda i, j: i > 1 and g[([i-1, i-1, i-1], [j-1, j, j+1])].sum() == 0,
        lambda i, j: i < g.shape[0] - 3 and g[([i+1, i+1, i+1], [j-1, j, j+1])].sum() == 0,
        lambda i, j: j > 1 and g[([i-1, i, i+1], [j-1, j-1, j-1])].sum() == 0,
        lambda i, j: j < g.shape[1] - 3 and g[([i-1, i, i+1], [j+1, j+1, j+1])].sum() == 0,
    ]

    act = [
        lambda i, j: (i-1, j),
        lambda i, j: (i+1, j),
        lambda i, j: (i, j-1),
        lambda i, j: (i, j+1),
    ]

    for i in range(1, g.shape[0]-2):
        for j in range(1, g.shape[1]-2):
            if g[i, j] == 0:
                continue

            if g[i-1:i+2, j-1:j+2].sum() < 2:
                prop[i, j] = 1
            elif cond[round % 4](i, j):
                prop[act[round % 4](i, j)] += 1
            elif cond[(round + 1) % 4](i, j):
                prop[act[(round + 1) % 4](i, j)] += 1
            elif cond[(round + 2) % 4](i, j):
                prop[act[(round + 2) % 4](i, j)] += 1
            elif cond[(round + 3) % 4](i, j):
                prop[act[(round + 3) % 4](i, j)] += 1
            else:
                prop[i, j] = 1

    for i in range(1, g.shape[0]-1):
        for j in range(1, g.shape[1]-1):
            if g[i, j] == 0:
                continue

            if g[i-1:i+2, j-1:j+2].sum() < 2:
                ng[i, j] = 1
            elif cond[round % 4](i, j):
                dest = act[round % 4](i, j)
                if prop[dest] == 1: ng[dest] = 1
                else: ng[i, j] = 1
            elif cond[(round + 1) % 4](i, j):
                dest = act[(round + 1) % 4](i, j)
                if prop[dest] == 1: ng[dest] = 1
                else: ng[i, j] = 1
            elif cond[(round + 2) % 4](i, j):
                dest = act[(round + 2) % 4](i, j)
                if prop[dest] == 1: ng[dest] = 1
                else: ng[i, j] = 1
            elif cond[(round + 3) % 4](i, j):
                dest = act[(round + 3) % 4](i, j)
                if prop[dest] == 1: ng[dest] = 1
                else: ng[i, j] = 1
            else:
                ng[i, j] = 1

    return ng

def p1(inp):
    g = inp
    # printgrid(g, ".#", True)
    for i in range(10):
        g = step(g, i)
        # printgrid(g, ".#", True)
        # print(f"This was after step {i+1}")

    xs = np.argwhere(g)
    x0, y0 = xs.min(axis=0)
    xn, yn = xs.max(axis=0) + 1

    return (xn - x0) * (yn - y0) - g.sum()

def p2(inp):
    g = inp
    # printgrid(g, ".#", True)
    i = 0
    while True:
        ng = step(g, i)
        # printgrid(ng, ".#", True)
        # print(f"This was after step {i+1}")
        i += 1
        if np.array_equal(g, ng):
            break
        g = ng

    return i

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 4045
# Solution part 2: 963

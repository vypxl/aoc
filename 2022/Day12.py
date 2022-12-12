#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return grid(inp, "SabcdefghijklmnopqrstuvwxyzE")

def p1(inp):
    S = next(zip(*np.where(inp == 0)))
    E = next(zip(*np.where(inp == 27)))
    inp[S] = 1

    d, _ = dijkstra_grid(inp, S, neighbours_straight, lambda v, n: inp[v] - inp[n] >= -1)

    return d[E]

def p2(inp):
    inp[inp == 0] = 1
    E = next(zip(*np.where(inp == 27)))

    d, _ = dijkstra_grid(inp, E, neighbours_straight, lambda v, n: inp[n] - inp[v] >= -1)

    return min(v for i, v in d.items() if inp[i] == 1)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 370
# Solution part 2: 363

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return grid(inp)

def p1(inp):
    w, h = inp.shape
    g = inp
    c = 0
    for i in range(w):
        for j in range(h):
            v = g[i,j]
            G = g - v
            xs = [G[i, j+1:], G[i, :j], G[i+1:, j], G[:i, j]]
            if any(all(x < 0) for x in xs): c+=1
    return c

def p2(inp):
    w, h = inp.shape
    g = inp
    m = 0
    for i in range(1, w-1):
        for j in range(1, h-1):
            v = g[i,j]
            xs = [g[i, j+1:], g[i, :j][::-1], g[i+1:, j], g[:i, j][::-1]]
            f = lambda x: len(list(it.takewhile(lambda a: a < v, x)))
            ys = [fx if fx == len(x) else 1+fx for x, fx in [(x, f(x)) for x in xs]]
            m = max(m, prod(ys))
    return m

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1814
# Solution part 2: 330786

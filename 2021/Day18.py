#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *
from math import floor, ceil

def parse(inp):
    def parseEx(l):
        xs = []
        nest = 0
        for c in l:
            if c == '[': nest += 1
            elif c == ']': nest -= 1
            elif c == ',': pass
            else: xs.append([int(c), nest])

        return xs

    return [parseEx(l) for l in inp.splitlines()]

def add(a, b):
    return [[v, n+1] for v, n in a + b]

def explode(n):
    for i, x in enumerate(n):
        v, nest = x
        if nest >= 5:
            l = [] if i == 0 else n[:i-1] + [[n[i-1][0] + v, n[i-1][1]]]
            r = [] if i >= len(n) - 2 else [[n[i+2][0] + n[i+1][0], n[i+2][1]]] + n[i+3:]

            return True, l + [[0, 4]] + r
    return False, n

def split(n):
    for i, x in enumerate(n):
        v, nest = x
        if v >= 10:
            return True, n[:i] + [[floor(v/2), nest+1], [ceil(v / 2), nest+1]] + n[i+1:]

    return False, n

def reduceN(n):
    exploded, nn = explode(n)
    if exploded: return reduceN(nn)

    splitted, nn = split(n)
    if splitted: return reduceN(nn)

    return n

def mag(n):
    S = [n[0]]
    for x in n[1:]:
        S.append(x)
        while len(S) >= 2 and S[-2][1] == S[-1][1]:
            r, ns = S.pop()
            l, _ = S.pop()
            S.append([3*l + 2*r, ns-1])

    return S.pop()[0]

def p1(inp):
    n = reduceN(inp[0])
    for l in inp[1:]:
        n = reduceN(add(n, l))
    return mag(n)

def p2(inp):
    m = 0
    for a in inp:
        for b in inp:
            m = max(m, mag(reduceN(add(a, b))))
    return m

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 4417
# Solution part 2: 4796


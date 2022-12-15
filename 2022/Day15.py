#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [nums(l) for l in inp.splitlines()]

def dist(a,b,c,d):
    return abs(a-c) + abs(b-d)

def p1(inp):
    row = 10
    row = 2000000
    pos = set()
    bs = set()

    for sx, sy, bx, by in inp:
        d = dist(sx, sy, bx, by)
        dy = abs(sy - row)
        r = d - dy
        if by == row: bs.add(bx)
        if r < 0: continue
        for x in range(sx - r, sx + r + 1):
            pos.add(x)

    return len(pos - bs)

def p2(inp, startx=0):
    MX = 4000000

    for row in range(startx, MX + 1):
        xs = []
        pos = []

        for sx, sy, bx, by in inp:
            d = dist(sx, sy, bx, by)
            dy = abs(sy - row)
            r = d - dy

            if r < 0: continue

            begin, end = sx - r, sx + r
            pos.append(range(begin, end))
            xs.append(begin-1)
            xs.append(end+1)

        for x in xs:
            for r in pos:
                if x < 0 or x > MX or x in r:
                    break
            else:
                Bx = x
                By = row
                return Bx * MX + By

    print("Not found :(")
    return None

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 5166077
# Solution part 2: 13071206703981
# Leaderboard: 882 / 605

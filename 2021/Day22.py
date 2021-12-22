#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [('on' in l, tuple(nums(l))) for l in inp.splitlines()]

def p1(inp):
    cube = np.zeros((101, 101, 101), dtype=int)

    for (on, (x1, x2, y1, y2, z1, z2)) in inp:
        x1 = max(x1, -50) + 50
        x2 = min(x2, 50) + 50
        y1 = max(y1, -50) + 50
        y2 = min(y2, 50) + 50
        z1 = max(z1, -50) + 50
        z2 = min(z2, 50) + 50
        cube[x1:x2+1, y1:y2+1, z1:z2+1] = 1 if on else 0

    return cube.sum()

def size(r):
    x1, x2, y1, y2, z1, z2 = r
    if x2 < x1 or y2 < y1 or z2 < z1: return 0
    return (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

def intersection(ra, rb):
    x1, x2, y1, y2, z1, z2 = ra
    x1_, x2_, y1_, y2_, z1_, z2_ = rb
    return (max(x1, x1_), min(x2, x2_), max(y1, y1_), min(y2, y2_), max(z1, z1_), min(z2, z2_))

def supersect(reg, regs):
    if len(regs) == 0: return 0
    return sum(
        size(regi) - supersect(regi, regs[j+1:])
        for j, regi in enumerate(map(lambda r: intersection(reg, r[1]), regs))
        if size(regi) > 0
    )

def p2(inp):
    instrs = list(reversed(inp))
    return sum(
        size(ins) - supersect(ins, instrs[:i])
        for i, (on, ins) in enumerate(instrs)
        if on
    )
 
def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 601104
# Solution part 2: 1262883317822267

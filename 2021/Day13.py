#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    dots, instrs = inp.split('\n\n')
    return np.array([nums(xs) for xs in dots.splitlines()]), structuredre(instrs, r"fold along ([xy])=(\d+)", (str, int))

def fold(dots, inst):
    xy, i = inst
    
    if xy == 'x': return set((x if x < i else 2 * i - x, y) for x, y in dots)
    else: return set((x, y if y < i else 2 * i - y) for x, y in dots)

def p1(inp):
    dots, instrs = inp
    return len(fold(dots, instrs[0]))

def p2(inp):
    dots, instrs = inp
    dots = reduce(fold, instrs, dots)

    return showgrid(grid_from_indices(dots))

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 661
# Solution part 2: PFKLKCFP
# Leaderboard: 289 / 3483 (stupid >=)

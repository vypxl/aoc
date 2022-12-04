#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [(set(range(a, b+1)), set(range(c,d+1))) for a,b,c,d in map(pnums, lines(inp))]

def p1(inp):
    return sum(1 for a,b in inp if a.issubset(b) or b.issubset(a))

def p2(inp):
    return sum(1 for a,b in inp if len(a.intersection(b)) != 0)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 450
# Solution part 2: 837

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [(ord(c) - ord('A'), ord(d) - ord('X')) for c, d in [x.split() for x in inp.splitlines()]]

def p1(inp):
    return sum(1 + b + ((b - a + 1) % 3) * 3 for a, b in inp)

def p2(inp):
    return p1((a, (a + b - 1) % 3) for a, b in inp)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 11449
# Solution part 2: 13187
# Leaderboard: 882 / 2769

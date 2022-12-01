#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return sorted([sum(nums(x)) for x in superlines(inp)])

def p1(inp):
    return inp[-1]

def p2(inp):
    return sum(inp[-3:])

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 67450
# Solution part 2: 199357
# Leaderboard: 882 / 605

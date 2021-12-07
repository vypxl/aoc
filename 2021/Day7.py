#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def p1(inp):
    return min(sum(np.abs(inp - i)) for i in range(max(inp)))

def p2(inp):
    return min(sum((x * (x + 1) // 2) for x in np.abs(inp - i)) for i in range(max(inp)))

def main():
    inp = np.array(nums(data()))
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 349769
# Solution part 2: 99540554
# Leaderboard: 534 / 1511

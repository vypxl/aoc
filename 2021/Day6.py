#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def calc(inp, n):
    memo = {}
    def calc_fish(f):
        if memo.get(f) is not None:
            return memo[f]
        fx = max(0, n - f - 1) // 7 + (1 if n - f > 0 else 0)

        x = 1
        for i in range(1, fx + 1):
            x += calc_fish(f + 2 + 7 * i)
        
        memo[f] = x
        return x

    ret = sum(calc_fish(f) for f in inp)
    return ret

def p1(inp):
    return calc(inp, 80)

def p2(inp):
    return calc(inp, 256)

def main():
    inp = nums(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 355386
# Solution part 2: 1613415325809

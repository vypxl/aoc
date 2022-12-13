#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *
import json
import functools

def parse(inp):
    return [[json.loads(x) for x in lines(pair)] for pair in superlines(inp)]

def compare(a, b):
    match a, b:
        case int(), int():
            return a - b
        case int(), list():
            return compare([a], b)
        case list(), int():
            return compare(a, [b])
        case list(), list():
            for i in range(len(b)):
                if i >= len(a):
                    return -1
                c = compare(a[i], b[i])
                if c == 0:
                    continue
                else:
                    return c
            return 1 if len(a) > len(b) else 0

    return 0

def p1(inp):
    return sum(i+1 for i, xs in enumerate(inp) if compare(xs[0], xs[1]) < 0)

def p2(inp):
    xs = flatten(inp) + [[[2]], [[6]]]
    ys = sorted(xs, key=functools.cmp_to_key(compare))
    a = ys.index([[2]]) + 1
    b = ys.index([[6]]) + 1
    return a * b

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 6415
# Solution part 2: 20056
# Leaderboard: 628 / 1354

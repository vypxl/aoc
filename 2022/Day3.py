#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return lines(inp)

def prio(c):
    if ord(c) >= ord('a') and ord(c) <= ord('z'):
        return ord(c) - ord('a') + 1
    if ord(c) >= ord('A') and ord(c) <= ord('Z'):
        return ord(c) - ord('A') + 27
    return 0

def p1(inp):
    sum = 0
    for x in inp:
        a = set(x[:len(x)//2])
        b = set(x[len(x)//2:])
        c = a.intersection(b)
        sum += prio(list(c)[0])
    return sum

def p2(inp):
    sum = 0
    for x in chunks(inp, 3):
        a = set(x[0])
        b = set(x[1])
        c = set(x[2])
        d = a.intersection(b)
        e = d.intersection(c)
        sum += prio(list(e)[0])
    return sum

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 8139
# Solution part 2: 2668
# Leaderboard: 882 / 605

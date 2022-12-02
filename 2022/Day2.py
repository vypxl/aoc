#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [x.split() for x in inp.splitlines()]

xs = {
    'A': 1,
    'B': 2,
    'C': 3,
    'X': 1,
    'Y': 2,
    'Z': 3,
}

def score(a, b):
    if a == 'A' and b == 'Y' or a == 'B' and b == 'Z' or a == 'C' and b == 'X': return 6
    if a == 'A' and b == 'X' or a == 'B' and b == 'Y' or a == 'C' and b == 'Z': return 3
    return 0

# def draw(a, b):

def win(a):
    if a == 'A': return 'Y'
    if a == 'B': return 'Z'
    if a == 'C': return 'X'

def draw(a):
    if a == 'A': return 'X'
    if a == 'B': return 'Y'
    if a == 'C': return 'Z'

def lose(a):
    if a == 'A': return 'Z'
    if a == 'B': return 'X'
    if a == 'C': return 'Y'

which = {
    'Z': win,
    'Y': draw,
    'X': lose,
}

def p1(inp):
    return sum(xs[b] + score(a, b) for a, b in inp)

def p2(inp):
    return sum(xs[which[b](a)] + score(a, which[b](a)) for a, b in inp)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 11449
# Solution part 2: 13187
# Leaderboard: 882 / 2769

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return lines(inp)

def prio(c):
    return 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'.index(c) + 1

def intersectRucksack(rucksack):
    return set.intersection(*(map(set, parts(rucksack, 2)))).pop()

def intersectBadge(badge):
    return set.intersection(*map(set, badge)).pop()

def p1(inp):
    return sum(prio(intersectRucksack(rucksack)) for rucksack in inp)

def p2(inp):
    return sum(prio(intersectBadge(badge)) for badge in chunks(inp, 3))

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 8139
# Solution part 2: 2668
# Leaderboard: 882 / 605

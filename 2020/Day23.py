#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    digits = [int(x) for x in list(inp) if x.isdigit()]
    xs = [None] * (len(digits) + 1)
    for i, d in enumerate(digits):
        xs[d] = digits[(i + 1) % len(digits)]
    return (digits[0], xs)

def get_order(cur, cups):
    res = []
    for _ in range(len(cups) - 1):
        res.append(cur)
        cur = cups[cur]
    return res

def play(start, cups, moves=100):
    cups = cups[:]
    m = len(cups) - 1
    skip = lambda n, c: c if n <= 0 else skip(n-1, cups[c])
    cur = start

    for _ in range(moves):
        pickup = [skip(n, cur) for n in range(1, 4)]
        cups[cur] = skip(4, cur)

        dest = cur - 1
        for _ in range(5):
            if dest <= 0:
                dest = m
            if dest in pickup:
                dest -= 1

        cups[pickup[-1]] = cups[dest]
        cups[dest] = pickup[0]
        cur = cups[cur]

    return cups

def p1(inp):
    cups = play(*inp)
    return ''.join(str(x) for x in get_order(1, cups))[1:]

def p2(inp):
    start, cups = inp
    ncups_orig = len(cups) - 1
    ncups   = 1000000
    nrounds = 10000000
    cups = cups + [x + 1 for x in range(ncups_orig + 1, ncups + 1)]
    cups[-1] = start
    cups[cups.index(start)] = ncups_orig + 1

    cups = play(start, cups, moves=nrounds)
    return cups[1] * cups[cups[1]]

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 89573246
# Solution part 2: 2029056128

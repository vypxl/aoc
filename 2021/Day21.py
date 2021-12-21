#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    x = nums(inp)
    return x[1], x[3]

def roll(die):
    return die % 100 + 1

def add(a, b, mod):
    return (a + b - 1) % mod + 1

def p1(inp):
    pa = inp[0]
    pb = inp[1]
    sa, sb = 0, 0
    die = 0
    turns = 0
    while True:
        die = roll(die)
        pa = add(pa, die, 10)
        die = roll(die)
        pa = add(pa, die, 10)
        die = roll(die)
        pa = add(pa, die, 10)
        turns += 3
        sa += pa
        if sa >= 1000: return sb * turns
        die = roll(die)
        pb = add(pb, die, 10)
        die = roll(die)
        pb = add(pb, die, 10)
        die = roll(die)
        pb = add(pb, die, 10)
        turns += 3
        sb += pb
        if sb >= 1000: return sa * turns

# Why is this fast enough
memo = {}
outcomes = [3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9]
def wins(a, b, sa, sb):
    if (a, b, sa, sb) in memo: return memo[(a, b, sa, sb)]

    wa, wb = 0, 0
    for d in outcomes:
        pa = add(a, d, 10)
        if sa + pa >= 21:
            wa += 1
        else:
            for e in outcomes:
                pb = add(b, e, 10)
                if sb + pb >= 21: wb += 1
                else:
                    wax, wbx = wins(pa, pb, sa + pa, sb + pb)
                    wa += wax
                    wb += wbx

    memo[(a, b, sa, sb)] = (wa, wb)
    return (wa, wb)

def p2(inp):
    return max(wins(inp[0], inp[1], 0, 0))

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 903630
# Solution part 2: 303121579983974
# Leaderboard: 1109 / 731

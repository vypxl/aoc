#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
import re
from util import data, lmap, nums

NEW = 0
CUT = 1
INC = 2


def slam(deck, instructions):
    for op, par in instructions:
        if op == NEW:
            deck = list(reversed(deck))
        elif op == CUT:
            deck = deck[par:] + deck[:par]
        elif op == INC:
            l = len(deck)
            newdeck = [None] * l
            pos = 0
            while 1:
                if not deck:
                    break
                newdeck[pos] = deck.pop(0)
                pos = (pos + par) % l
            deck = newdeck
    return deck


# Sorry myself, I do not want to think about math when shuffling cards.
# Thanks to metalim: https://github.com/metalim/metalim.adventofcode.2019.python/blob/master/22_cards_shuffle.ipynb


def make_poly(l, rules):
    m, b = 1, 0
    for op, par in reversed(rules):
        if op == NEW:
            m = -m
            b = l - b - 1
        elif op == CUT:
            b = (b + par) % l
        elif op == INC:
            z = pow(par, l - 2, l)
            m = m * z % l
            b = b * z % l
    return m, b


def modpow_poly(m, b, l, count):
    if count == 0:
        return 1, 0
    elif count % 2 == 0:
        return modpow_poly(m * m % l, (m * b + b) % l, l, count // 2)
    else:
        c, d = modpow_poly(m, b, l, count - 1)
        return m * c % l, (m * d + b) % l


def slam2(l, count, rules, pos):
    m, b = make_poly(l, rules)
    m, b = modpow_poly(m, b, l, count)
    return (m * pos + b + l) % l


def p1(inp):
    return slam(list(range(10007)), inp).index(2019)


def p2(inp):
    LEN = 119315717514047
    COUNT = 101741582076661
    return slam2(LEN, COUNT, inp, 2020)


def parse(inp):
    instructions = []

    for l in inp.strip().splitlines():
        if re.match(r"deal into new stack", l):
            instructions.append((NEW, None))
        elif re.match(r"cut -?\d+", l):
            instructions.append((CUT, nums(l)[0]))
        elif re.match(r"deal with increment \d+", l):
            instructions.append((INC, nums(l)[0]))

    return instructions


def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 6417
# Solution part 2: 98461321956136

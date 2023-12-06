#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def loc(k, m):  # noqa
    i = -1
    for mapper in m:
        i += 1
        for r in mapper:
            d, s, l = r
            if s <= k < s + l:
                k = k - s + d
                break
    return k


def parse(inp):  # noqa
    a = superlines(inp)
    seeds = nums(a[0])
    b = [[tuple(nums(l)) for l in lines(x)[1:]] for x in a[1:]]

    return seeds, b


def p1(inp):  # noqa
    s, m = inp
    xs = [loc(k, m) for k in s]
    # print(xs)
    return min(xs)


def merge(ranges):  # noqa
    nr = set()
    for a, b in ranges:
        found = False
        for x, y in ranges:
            if x < a < b <= y or x <= a < b < y or b <= a:
                found = True
        if not found:
            nr.add((a, b))

    nr2 = set()
    i, j = 0, 0
    for a, b in sorted(nr, key=fst):
        if a >= j:
            if j - i > 0:
                nr2.add((i, j))
            i = a
        j = b
    if j - i > 0:
        nr2.add((i, j))

    return nr2


def p2(inp):  # noqa
    s, m = inp
    ranges = set()
    for a, b in chunks(s, 2):
        ranges.add((a, a + b))

    for mapper in m:
        mapper = sorted(mapper, key=lambda x: x[1])
        nr = set()
        su = 0
        for fr, until in ranges:
            i = fr
            csu = 0
            for r in mapper:
                d, s, l = r
                if i >= s + l:
                    continue
                if i < s:
                    nr.add((i, s))
                    su += s - i
                    csu += s - i
                    i += s - i

                ml = min(until, s + l) - max(i, s)
                to_add = (i - s + d, i - s + d + ml)
                su += ml
                csu += ml
                nr.add(to_add)
                i += ml
                if i >= until:
                    break
            if i < until:
                su += until - i
                csu += until - i
                nr.add((i, until))

        ranges = merge(nr)

    xs = [r[0] for r in ranges]
    return min(xs)


def main():  # noqa
    print("====")
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 214922730
# Solution part 2: 148041808
# Leaderboard: 902 / 3957

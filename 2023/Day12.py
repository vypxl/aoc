#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return [(a, nums(b)) for (a, b) in [l.split() for l in lines(inp)]]


dp = {}


def counts(l, gs):
    k = str(l) + str(gs)
    if k in dp:
        return dp[k]

    ret = -999999
    if len(l) == 0:
        if len(gs) == 1 and gs[0] == 0 or len(gs) == 0:
            ret = 1
        else:
            ret = 0
    else:
        head, tail = l[0], l[1:]
        if head == "#":
            if len(gs) == 0:
                ret = 0
            else:
                g0, gtail = gs[0], gs[1:]
                if g0 == 0:
                    ret = 0
                else:
                    ret = counts(tail, [-(abs(gs[0]) - 1)] + gs[1:])
        elif head == ".":
            if len(gs) > 0 and gs[0] == 0:
                ret = counts(tail, gs[1:])
            elif len(gs) > 0 and gs[0] < 0:
                ret = 0
            else:
                ret = counts(tail, gs)
        elif l[0] == "?":
            ret = counts("#" + tail, gs) + counts("." + tail, gs)

    dp[k] = ret

    return ret


def p1(inp):  # noqa
    return sum(counts(*x) for x in inp)


def p2(inp):  # noqa
    return sum(counts("?".join([a] * 5), b * 5) for a, b in inp)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 6981
# Solution part 2: 4546215031609
# Leaderboard: 303 / 817

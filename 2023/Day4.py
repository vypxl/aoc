#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    ls = [l.split(":")[1].split("|") for l in lines(inp)]
    return [(nums(a), nums(b)) for a, b in ls]


def solve(inp):  # noqa
    p1, p2 = 0, 0
    card_counts = [1] * len(inp)
    for i, (winning, numbers) in enumerate(inp):
        matching = set(winning).intersection(set(numbers))
        match_count = len(matching)

        # part 1
        if match_count > 0:
            p1 += 2 ** (match_count - 1)

        # part 2
        for j in range(i + 1, i + 1 + match_count):
            card_counts[j] += card_counts[i]
        p2 += card_counts[i]

    return p1, p2


def main():  # noqa
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")


if __name__ == "__main__":
    main()

# Solution part 1: 27454
# Solution part 2: 6857330
# Leaderboard: 990 / 565

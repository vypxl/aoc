#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from functools import reduce
from util import *


def parse(inp):  # noqa
    return re.split(r",|\n", inp)


def hash(x):  # noqa
    return reduce(lambda h, c: (h + ord(c)) * 17 % 256, x, 0)


def p1(inp):  # noqa
    return sum(hash(x) for x in inp)


def run_inst(hm, inst):  # noqa
    if "=" in inst:
        k, v = inst.split("=")
        v = int(v)
        box_idx = hash(k)
        entry = hm[box_idx]
        for i, x in enumerate(entry):
            if x[0] == k:
                entry[i] = (k, v)
                return
        entry.append((k, v))
    if "-" in inst:
        k = inst[:-1]
        box_idx = hash(k)
        hm[box_idx] = [x for x in hm[box_idx] if x[0] != k]


def p2(inp):  # noqa
    hm = [[] for _ in range(256)]
    for inst in inp:
        run_inst(hm, inst)

    return sum(
        (i + 1) * sum((j + 1) * v for j, (_, v) in enumerate(entry))
        for i, entry in enumerate(hm)
    )


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 516804
# Solution part 2: 231844
# Leaderboard: 1181 / 817

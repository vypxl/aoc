#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from math import ceil
import numpy as np
from toolz.curried import pipe, map
from util import data, lmap

PAT = [0, 1, 0, -1]


def fft(thing, phases=1):
    patterns = pipe(
        thing,
        len,
        range,
        map(lambda i: np.repeat(PAT, i + 1)),
        map(lambda x: np.tile(x, ceil(len(thing) / len(x)) + 1)[1 : len(thing) + 1]),
        list,
        np.array,
    )

    for _ in range(phases):
        thing = np.mod(np.absolute((thing * patterns).sum(axis=1)), 10)
    return thing


def fft_cheat(thing, phases=1):
    offset = int("".join(map(str, thing[:7])))
    if offset < len(thing) // 2:
        raise "Cannot cheat with offset less than half of the input length!"

    thing = np.tile(thing, 10000)[offset:]

    for _ in range(phases):
        thing = np.mod(thing[::-1].cumsum()[::-1], 10)
    return thing


def p1(inp):
    return "".join(map(str, fft(inp, 100)[:8]))


def p2(inp):
    hsh = fft_cheat(inp, 100)
    return "".join(map(str, hsh[:8]))


def main():
    inp = lmap(int, data().strip())
    inp = np.array(inp, dtype=np.int8)

    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 19944447
# Solution part 2: 81207421

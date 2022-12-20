#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return nums(inp)

def round(xs, indices, moves):
    for idx, x in enumerate(moves):
        i0 = indices.index(idx)
        i = i0
        dir = -1 if x < 0 else 1

        for _ in range(abs(x) % (len(xs) - 1)):
            j = (i + dir) % len(xs)
            tmp = xs[i]
            xs[i] = xs[j]
            xs[j] = tmp

            tmp = indices[i]
            indices[i] = indices[j]
            indices[j] = tmp

            i = j

def get_sol(xs):
    o = xs.index(0)
    i1 = (o + 1000) % len(xs)
    i2 = (o + 2000) % len(xs)
    i3 = (o + 3000) % len(xs)
    return xs[i1] + xs[i2] + xs[i3]

def p1(inp):
    moves = inp[:]
    xs = inp[:]
    indices = list(range(len(xs)))

    round(xs, indices, moves)
    return get_sol(xs)

def p2(inp):
    xs = [x * 811589153 for x in inp]
    moves = xs[:]
    indices = list(range(len(xs)))

    for mix in range(10):
        round(xs, indices, moves)

    return get_sol(xs)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 3466
# Solution part 2: 9995532008348

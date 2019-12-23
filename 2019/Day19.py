#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
import itertools as it
from toolz.curried import pipe, map
from util import data, lmap
from intcode import IntComputer

PRINT_ASM = False


def p1(inp, debug=False):
    return pipe(
        it.product(range(50), repeat=2),
        map(list),
        map(lambda coord: IntComputer(inp, name="Tractorbeam", inp=coord, debug=debug).run()[0]),
        sum
    )

def p2(inp, debug=False):
    x = 0
    y = 500 # arbitrary optimization

    in_beam = lambda x, y: IntComputer(inp, name="Tractorbeam", inp=[x, y], debug=debug).run()[0]

    while 1:
        y += 1
        while 1:
            if in_beam(x, y):
                if in_beam(x+99, y-99):
                    return x * 10000 + (y - 99)
                else:
                    x -= 3 # arbitrary optimization
                    break
            x += 1


def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp, debug=PRINT_ASM)}")
    print(f"Solution for part 2:\n{p2(inp, debug=PRINT_ASM)}")

if __name__ == "__main__":
    main()

# Solution part 1: 126
# Solution part 2: 11351625

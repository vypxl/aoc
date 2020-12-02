#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *
from toolz.curried import *

def p1(inp):
    return pipe(inp,
        filter(lambda x: x[2].count(x[1]) in x[0]),
        list, len
    )

def p2(inp):
    return pipe(inp,
        filter(lambda x: (x[2][x[0].start - 1] == x[1]) ^ (x[2][x[0].stop - 2] == x[1])),
        list, len
    )

def parse(inp):
    parseRange = lambda x: pipe(x.split('-'), map(int), list, lambda x: range(x[0], x[1] + 1))
    return pipe(inp, map(lambda x: x.split(' ')), map(lambda x: [parseRange(x[0]), x[1][0], x[2]]), list)

def main():
    inp = parse(data_lines())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 580
# Solution part 2: 611

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

val = { '.': 0, '#': 1 }

def parse(inp):
    return np.array(lmap(lambda x: lmap(lambda y: val[y], x), inp))

def run(grid, right, down):
    downs = it.takewhile(lambda x: x < grid.shape[0], it.count(down, down))
    rights = it.count(right, right)
    coords = np.remainder(np.array(list(zip(downs, rights))), grid.shape).transpose()
    return grid[coords[0], coords[1]].sum()

def p1(inp):
    return run(inp, 3, 1)

def p2(inp):
    return pipe([(1,1), (3,1), (5,1), (7,1), (1,2)],
        map(lambda x: run(inp, *x)),
        list,
        np.prod
    )

def main():
    inp = parse(data_lines())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 191
# Solution part 2: 1478615040

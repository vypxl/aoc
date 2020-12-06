#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return compose(list, map(compose(list, map(set), str.splitlines)))(inp.split('\n\n'))

def p1(inp):
    return compose(sum, map(compose(count, reduce(set.union))))(inp)

def p2(inp):
    return compose(sum, map(compose(count, reduce(set.intersection))))(inp)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 6551
# Solution part 2: 3358

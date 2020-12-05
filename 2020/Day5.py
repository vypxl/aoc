#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return list(map(lambda x: int(x, 2), tr(inp, 'BRFL', '1100').splitlines()))

def p1(inp):
    return max(inp)

def p2(inp):
    return set(range(min(inp), max(inp) + 1)).difference(inp).pop()

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 926
# Solution part 2: 657

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def get_marker(inp, n):
    for i in range(len(inp)):
        if len(set(inp[i:i+n])) == n:
            return i + n
    return None

def p1(inp):
    return get_marker(inp, 4)

def p2(inp):
    return get_marker(inp, 14)

def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1598
# Solution part 2: 2414

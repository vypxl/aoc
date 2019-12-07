#! /usr/bin/env python
from util import (nums, data, lmap)
from intcode import IntComputer

def p1(inp):
    return IntComputer.run_direct(inp, [1], debug=True)

def p2(inp):
    return IntComputer.run_direct(inp, [5], debug=True)

def main():
    inp = data()
    p1result, p2result = p1(inp), p2(inp)

    print(f"Solution for part 1:\n{p1result[-1]}")
    print(f"Solution for part 2:\n{p2result[-1]}")

if __name__ == "__main__":
    main()

# Solution part 1: 15508323
# Solution part 2: 9006327

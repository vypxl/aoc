#! /usr/bin/env python
from itertools import permutations
from util import data
from intcode import IntComputer

# If the program should print out executed assembly instructions
PRINT_ASM = True

def p1(inp, debug=False):
    return IntComputer.run_direct(inp, [1], "Part 1", debug=debug)

def p2(inp, debug=False):
    return IntComputer.run_direct(inp, [2], "Part 2", debug=debug)

def main():
    inp = data()

    p1result, p2result = p1(inp, debug=PRINT_ASM), p2(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")

if __name__ == "__main__":
    main()

# Solution part 1: 3518157894
# Solution part 2: 80379

#! /usr/bin/env python
from itertools import permutations
from util import data
from intcode import IntComputer

# If the program should print out executed assembly instructions
PRINT_ASM = False


def p1(inp, debug=False):
    print(f"Running {inp} with input [1]...\n")
    return IntComputer(inp, inp=[1], name="Part 1", debug=debug).run()


def p2(inp, debug=False):
    print(f"Running {inp} with input [2]...\n")
    return IntComputer(inp, inp=[2], name="Part 2", debug=debug).run()


def main():
    inp = data()

    p1result, p2result = p1(inp, debug=PRINT_ASM), p2(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")


if __name__ == "__main__":
    main()

# Solution part 1: 3518157894
# Solution part 2: 80379

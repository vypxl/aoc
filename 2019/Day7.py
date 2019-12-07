#! /usr/bin/env python
from itertools import permutations
from util import data
from intcode import IntComputer

# If the program should print out executed assembly instructions
PRINT_ASM = False

def p1(inp, debug=False):
    m = 0
    for setting in permutations([0,1,2,3,4]):
        setting = [[s] for s in setting]
        setting[0] = setting[0] + [0]

        out = IntComputer.pipe(list(zip([inp] * 5, setting)), name="Amplifier", debug=debug)
        if out[0] > m:
            m = out[0]

    return m

def p2(inp, debug=False):
    m = 0
    for setting in permutations([5,6,7,8,9]):
        setting = [[s] for s in setting]
        setting[0] = setting[0] + [0]

        out = IntComputer.pipe(list(zip([inp] * 5, setting)), loop=True, name="Amplifier", debug=debug)
        if out[0] > m:
            m = out[0]

    return m

def main():
    inp = data()

    p1result, p2result = p1(inp, debug=PRINT_ASM), p2(inp, debug=PRINT_ASM)

    print(f"Solution for part 1:\n{p1result}")
    print(f"Solution for part 2:\n{p2result}")

if __name__ == "__main__":
    main()

# Solution part 1: 99376
# Solution part 2: 8754464

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return splittedlines(inp)

def solve(inp):
    c = 1
    o = 1
    x = 1
    strength = 0
    s = ""

    def draw():
        nonlocal c, o, x, strength, s

        if (c + 20) % 40 == 0:
            strength += c * x

        if o in range(x, x+3):
            s += '█'
        else:
            s += ' '
        o += 1

        if (c) % 40 == 0:
            s += ('\n')
            o = 1

    for l in inp:
        draw()
        c += 1
        if len(l) == 2:
            draw()
            x += int(l[1])
            c += 1

    return strength, s

def main():
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 14540
# Solution part 2: EHZFZHCZ

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def p1(inp):
    d = 0
    p = 0
    for xs in inp:
        cmd, x = xs
        
        if cmd == 'forward':
            p += x
        if cmd == 'down':
            d += x
        if cmd == 'up':
            d -= x
    return d * p

def p2(inp):
    d = 0
    p = 0
    a = 0
    for xs in inp:
        cmd, x = xs
        
        if cmd == 'forward':
            p += x
            d += a*x
        if cmd == 'down':
            a += x
        if cmd == 'up':
            a -= x
    return d * p

def main():
    inp = structured(data(), (str, int))

    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1804520
# Solution part 2: 1971095320

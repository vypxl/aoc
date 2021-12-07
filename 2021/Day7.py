#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def p1(inp):
    m = 239485778978
    for i in range(np.max(inp)):
        x = np.sum(np.abs(inp - i))
        m = min(m, x)
    return m

def p2(inp):
    m = 239485778978
    for i in range(np.max(inp)):
        xs = np.abs(inp - i)
        
        x = 0
        for a in xs:
            x += (a * (a+1)) // 2

        m = min(m, x)
    return m

def main():
    inp = np.array(nums(data()))
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 349769
# Solution part 2: 99540554

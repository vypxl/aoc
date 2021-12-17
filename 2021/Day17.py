#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def solve(inp):
    tx0, tx1, ty0, ty1 = inp
    txr = range(tx0, tx1+1)
    tyr = range(ty0, ty1+1)

    my = 0
    count = 0
    for vx in range(tx1 + 10):
        for vy in range(ty0, tx1 + 10):
            cvx, cvy = vx, vy
            tmy = 0
            S = (0, 0)
            step = 0
            while True:
                step += 1
                S = (S[0] + cvx, S[1] + cvy)
                if S[1] < ty0 and cvy < 0: break
                if S[0] > tx1: break
                cvx -= 1 if cvx > 0 else 0 if cvx == 0 else -1
                cvy -= 1
                tmy = max(tmy, S[1])
                if S[0] in txr and S[1] in tyr:
                    my = max(my, tmy)
                    count += 1
                    break

    return my, count

def main():
    inp = nums(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 3160
# Solution part 2: 1928

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

digits = "012=-"
values = [0, 1, 2, -2, -1]
need_carry = [False, False, False, True, True]

def snafu_to_i(n):
    return sum(values[digits.index(d)] * 5 ** i for i, d in enumerate(reversed(n)))

def i_to_snafu(n):
    k = ""
    while n > 0:
        n, r = divmod(n, 5)
        if need_carry[r]:
            n += 1
        k = digits[r] + k

    return k

def solve(inp):
    return i_to_snafu(sum(snafu_to_i(x) for x in lines(inp)))

def main():
    inp = data()
    print(f"Solution for part 1:\n{solve(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 2-02===-21---2002==0
# Leaderboard: 457

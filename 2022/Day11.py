#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    monkeys = []
    for raw_monkey in superlines(inp):
        ls = raw_monkey.splitlines()[1:]
        opline = ls[1]
        opNum = nums(opline)
        if not nums(opline):
            op = lambda x: x*x
        elif '*' in opline:
            op = lambda x, c=opNum[0]: c * x
        else:
            op = lambda x, c=opNum[0]: c + x
        monkeys.append([
                           op,
                           nums(ls[2])[0],
                           nums(ls[3])[0],
                           nums(ls[4])[0],
                           nums(ls[0]),
                       ])

    return monkeys

def solve(inp, rounds, shouldDivide):
    cs = [0] * len(inp)
    mod = prod(x[1] for x in inp)
    for _ in range(rounds):
        for mon, _ in enumerate(inp):
            op, div, t, f, items = inp[mon]
            inp[mon][-1] = []
            for item in items:
                cs[mon] += 1
                item = op(item)
                if shouldDivide:
                    item = item // 3
                if item % div == 0:
                    inp[t][-1].append(item % mod)
                else:
                    inp[f][-1].append(item % mod)

    return prod(sorted(cs)[-2:])

def p1(inp):
    return solve(inp, 20, True)

def p2(inp):
    return solve(inp, 10000, False)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(parse(data()))}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 61503
# Solution part 2: 14081365540

#! /usr/bin/env python
import math
import re
from util import data, lmap

def calc(reactions, src="FUEL", dest="ORE", stock={}, multiplier=1):
    stack = []
    cost = 0

    stack.append((multiplier, src))
    while len(stack) != 0:
        _qty, chem = stack.pop()

        if not chem in stock:
            stock[chem] = 0

        qty = max([0, _qty - stock[chem]])
        stock[chem] = max([0, stock[chem] - _qty])

        if chem == dest:
            cost += qty
            continue

        ed, (pqty, _) = next(x for x in reactions if x[1][1] == chem)
        actqty = math.ceil(qty / pqty)
        for eqty, e in ed:
            stack.append((eqty * actqty, e))


        stock[chem] += (actqty * pqty) - qty

    return cost

def p1(reactions):
    return calc(reactions)

def p2(reactions):
    available = 1000000000000
    stock = {}
    count = 0

    stepsize = 1000000
    while 1:
        newstock = stock.copy()
        cost = calc(reactions, stock=newstock, multiplier=stepsize)
        if available - cost < 0:
            if stepsize == 1:
                break
            stepsize = stepsize // 10
        else:
            available -= cost
            stock = newstock
            count += stepsize

    return count

def parse(s):
    def parseIng(x):
        q, c = x.split(" ")
        return (int(q), c)

    ret = [lmap(lambda x: parseIng(x.strip()), re.split(r"=>|,", l)) for l in s.splitlines()]
    return [(l[:-1], l[-1]) for l in ret]


def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 168046
# Solution part 2: 6972986

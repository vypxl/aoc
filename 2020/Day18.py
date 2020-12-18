#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

# from https://code.activestate.com/recipes/384122/
class Infix:
    def __init__(self, function):
        self.function = function
    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __or__(self, other):
        return self.function(other)
    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __rshift__(self, other):
        return self.function(other)
    def __call__(self, value1, value2):
        return self.function(value1, value2)

def p1(inp):
    return sum(
        eval(expr) for expr in 
        [expr.replace('+', '|plus|').replace('*', '|mul|') for expr in inp]
    )

def p2(inp):
    return sum(
        eval(expr) for expr in 
        [expr.replace('+', '<<plus>>').replace('*', '|mul|') for expr in inp]
    )

def main():
    inp = data_lines()

    globals()['plus'] = Infix(lambda x, y: x + y)
    globals()['mul'] = Infix(lambda x, y: x * y)

    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 9535936849815
# Solution part 2: 472171581333710

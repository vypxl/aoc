#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

## Reverse engineered input:

# z = 0
# z = i1  + 26*z + 6  if i1  != z%26 + 11 else z
# z = i2  + 26*z + 14 if i2  != z%26 + 13 else z
# z = i3  + 26*z + 14 if i3  != z%26 + 15 else z
# z = i4  + z    + 10 if i4  != z%26 - 8  else z//26
# z = i5  + 26*z + 9  if i5  != z%26 + 13 else z
# z = i6  + 26*z + 12 if i6  != z%26 + 15 else z
# z = i7  + z    + 8  if i7  != z%26 - 11 else z//26
# z = i8  + z    + 13 if i8  != z%26 - 4  else z//26
# z = i9  + z    + 12 if i9  != z%26 - 15 else z//26
# z = i10 + 26*z + 6  if i10 != z%26 + 14 else z
# z = i11 + 26*z + 9  if i11 != z%26 + 14 else z
# z = i12 + z    + 15 if i12 != z%26 - 1  else z//26
# z = i13 + z    + 4  if i13 != z%26 - 8  else z//26
# z = i14 + z    + 10 if i14 != z%26 - 14 else z//26

# This validation function was reverse engineered from my input by using SymPy.
# You can see the code I wrote for that down below.
# This function only takes a brute-forceable 7-digit number and
# returns True if it is valid, as only 7 digits are unbound by the algorithm.
# The other 7 digits are constrained by the algorithm and can be computed from the others.
def valid(n):
    s = str(n)
    if '0' in s or len(s) != 7: return None
    n = [int(x) for x in str(n)]
    
    # digits 3, 6, 7, 8, 11, 12 and 13 are bound, reducing the searchspace
    i = i = [n[0], n[1], n[2], 0, n[3], n[4], 0, 0, 0, n[5], n[6], 0, 0, 0]
    z = i[0] + 6
    z = i[1] + 26*z + 14
    z = i[2] + 26*z + 14
    i[3] = z%26 - 8; z = z // 26
    z = i[4] + 26*z + 9
    z = i[5] + 26*z + 12
    i[6] = z%26 - 11; z = z // 26
    i[7] = z%26 - 4; z = z // 26
    i[8] = z%26 - 15; z = z // 26
    z = i[9] + 26*z + 6
    z = i[10] + 26*z + 9
    i[11] = z%26 - 1; z = z // 26
    i[12] = z%26 - 8; z = z // 26
    i[13] = z%26 - 14; z = z // 26

    if (any(x <= 0 or x > 9 for x in i)): return None
    else: return int(''.join(map(str, i)))

def p1():
    for i in reversed(range(1111111, 10000000)):
        if valid(i) is not None:
            return valid(i)

def p2():
    # for i in range(1111111, 10000000):
    for i in range(9111111, 10000000): # Speedup, works for my input
        if valid(i) is not None:
            return valid(i)

###################################################################
# START Code used for reverse engineering the validation function #
# No quality guarantee                                            #
###################################################################

from math import *
from sympy import *

def parse(inp):
    return [tuple(l.split()) for l in inp.splitlines()]

regI = { 'w': 0, 'x': 1, 'y': 2, 'z': 3 }
def operate(op, p1, p2, regs, inp):
    v1 = regs[regI[p1]]
    v2 = None if p2 is None else (
        int(p2) if p2.isnumeric() or p2[1:].isnumeric() else regs[regI[p2]])

    res = None

    if op == 'inp':
        res = next(inp)
    elif op == 'add':
        res = v1 + v2
    elif op == 'mul':
        res = v1 * v2
    elif op == 'div':
        res = v1 // v2
    elif op == 'mod':
        res = v1 % v2
    elif op == 'eql':
        res = Piecewise((1, Eq(v1, v2)), (0, True))

    regs[regI[p1]] = res

def run(inp):
    indices = [i for i, x in enumerate(inp) if x[0] == 'inp']
    prog = [inp[a:b] for a, b in zip([0] + indices, indices + [len(inp)])][1:]
    for i, p in enumerate(prog):
        print(f"{i+1}: ", end='')
        n = iter([symbols('i', real=True)])
        regs = list(symbols('w x y z', real=True))
        for j, ins in enumerate(p):
            if len(ins) < 3: ins = ins + (None,)
            op, p1, p2 = ins
            operate(op, p1, p2, regs, n)
        regs = list(map(simplify, regs))
        print(f"{regs[3]}")

#################################################################
# END code used for reverse engineering the validation function #
#################################################################

def main():
    print(f"Solution for part 1:\n{p1()}")
    print(f"Solution for part 2:\n{p2()}")

if __name__ == "__main__":
    main()

# Solution part 1: 99394899891971
# Solution part 2: 92171126131911

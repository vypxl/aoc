#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    names = []
    exprs = {}

    prog = "from z3 import *\n"

    for l in inp.splitlines():
        name, val = l.split(': ')
        names.append(name)
        exprs[name] = val
        prog += f"{name} = Real('{name}')\n"

    prog += "s = Solver()\n"

    for name in names:
        if name not in ['root', 'humn']:
            prog += f"s.add({name} == {exprs[name]})\n"

    return exprs['root'], exprs['humn'], prog

def solve(prefix, custom_code, value_of_interest):
    prog = prefix + custom_code + f"s.check()\nresult = s.model()[{value_of_interest}]\n"
    locs = {}
    exec(prog, globals(), locs)
    return locs['result']

def p1(inp):
    root, humn, prog = inp
    code = f"s.add(humn == {humn})\ns.add(root == {root})\n"

    return solve(prog, code, 'root')

def p2(inp):
    root, _, prog = inp
    a, _, b = root.split(' ')
    code = f"s.add({a} == {b})\n"

    return solve(prog, code, 'humn')

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 87457751482938
# Solution part 2: 3221245824363

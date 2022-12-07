#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def gettree(tree, path):
    for p in path:
        tree = tree[p]
    return tree

def parse(inp):
    path = ['/']
    tree = {'/': {}}
    for l in lines(inp)[1:]:
        s = l.split()
        if l.startswith("$"):
            if l.startswith("$ cd"):
                cd = s[2]
                if cd == "..":
                    path.pop()
                else:
                    path.append(cd)
        else:
            a, b = s
            if a == 'dir':
                gettree(tree, path)[b] = {}
            else:
                gettree(tree, path)[b] = int(a)

    return tree

def solve(tree):
    p1 = 0
    sizes = []
    def size(tree):
        nonlocal p1, sizes
        s = 0
        for v in tree.values():
            if isinstance(v, int):
                s += v
            else:
                s_v = size(v)
                s += s_v
                sizes.append(s_v)

        if s <= 100000:
            p1 += s

        return s

    total = size(tree)
    needed = 30000000 + total - 70000000
    p2 = min(x for x in sizes if x >= needed)
    return (p1, p2)

def main():
    inp = parse(data())
    p1, p2 = solve(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 1453349
# Solution part 2: 2948823
# Leaderboard: 954 / 1525

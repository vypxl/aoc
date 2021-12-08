#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [(a.split(), b.split()) for a, b in [l.split(" | ") for l in inp.splitlines()]]

def p1(inp):
    return sum(len([x for x in xs if len(x) in [2,3,4,7]]) for _, xs in inp)

# 000
# 1 2
# 333
# 4 5
# 666
def get_entry(a, b):
    whichd = {
        (0, 1, 2, 4, 5, 6): 0,
        (2, 5): 1,
        (0, 2, 3, 4, 6): 2,
        (0, 2, 3, 5, 6): 3,
        (1, 2, 3, 5): 4,
        (0, 1, 3, 5, 6): 5,
        (0, 1, 3, 4, 5, 6): 6,
        (0, 2, 5): 7,
        (0, 1, 2, 3, 4, 5, 6): 8,
        (0, 1, 2, 3, 5, 6): 9,
    }
    which = { "a": -1, "b": -1, "c": -1, "d": -1, "e": -1, "f": -1, "g": -1 }
    def get(d):
        segs = tuple(sorted(which[x] for x in d))

        return whichd[segs]
    def getn(n):
        return int("".join(str(get(d)) for d in n))
    
    candidates = [set("abcdefg") for _ in range(7)]

    j = 0
    while any(which[x] == -1 for x in which):
        j += 1
        for d in a + b:
            to_update = []
            if len(d) == 2:
                to_update.append((2, d))
                to_update.append((5, d))
            elif len(d) == 3:
                to_update.append((0, d))
                to_update.append((2, d))
                to_update.append((5, d))
            elif len(d) == 4:
                to_update.append((1, d))
                to_update.append((2, d))
                to_update.append((3, d))
                to_update.append((5, d))
            elif len(d) == 5:
                to_update.append((0, d))
                to_update.append((3, d))
                to_update.append((6, d))
            elif len(d) == 6:
                to_update.append((0, d))
                to_update.append((1, d))
                to_update.append((5, d))
                to_update.append((6, d))
            
            for s, cs in to_update:
                candidates[s].intersection_update(cs)

        for i, xs in enumerate(candidates):
            if len(xs) == 1:
                found = xs.pop()
                which[found] = i
                xs.update(set("-"))
                for j, cs in enumerate(candidates):
                    if i != j:
                        cs.difference_update(set(found))

    return getn(b)

def p2(inp):
    return sum(get_entry(a, b) for a, b in inp)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# This is very ugly, but I will not improve it as I do not want to.

# Solution part 1: 488 
# Solution part 2: 1040429

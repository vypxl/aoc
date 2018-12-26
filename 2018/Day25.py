#! /usr/bin/env python
import re
from itertools import starmap

def dist(a, b):
    return sum(starmap(lambda c1, c2: abs(c1 - c2), zip(a, b)))

class Group():
    def __init__(self, initial):
        self.points = [initial]
        self.merged = False
    def insert(self, p):
        self.points.append(p)
    def merge(self, other):
        for p in self.points:
            for p2 in other.points:
                # If any point is in constellation with any point in the other group, the groups are mergeable
                if dist(p, p2) <= 3:
                    self.points.extend(other.points)
                    other.merged = True
                    return True
        return False

    def __contains__(self, p):
        for x in self.points:
            if dist(p, x) <= 3: return True
        return False
    def __eq__(self, other):
        return self.points == other.points and self.merged == other.merged

f = open("25.in").read()
groups = list(map(lambda x: Group(tuple(map(int, re.findall(r"[-\d]+", x)))), f.split('\n')[:-1]))

while True:
    didsth = False
    for g in groups:
        if g.merged: continue
        for g2 in groups:
            if g2.merged or g == g2: continue
            didsth = g.merge(g2) or didsth
    groups = list(filter(lambda g: not g.merged, groups))
    if not didsth: break
print(f"Solution for part 1:\n{len(groups)}")

# This is the shortest an most inefficient version I can think of right now. I really ought to look into set unions. I halfed the lines and tripeled the runtime ^^
# Solution part 1: 318

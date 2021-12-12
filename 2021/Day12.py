#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    E = structuredre(inp, r"(\w+)-(\w+)", (str, str))
    return set((u, v) for u, v in E + [(v, u) for u, v in E] if u != "end" and v != "start")

def dfs(E, vert = "start", before = ["start"], max = 1, chosen = None):
    paths = []

    for u, v in E:
        if u != vert: continue
        if v == "end":
            paths += [before + [v]]
            continue

        v_is_large = v[0].isupper()
        v_count = before.count(v)

        if v_is_large or v_count == 0 or (v_count < max and chosen in [None, v]):
            c = v if chosen is None and not v_is_large and v_count == 1 else chosen
            paths += dfs(E, v, before + [v], max, c)

    return paths

def p1(inp):
    return len(dfs(inp))

def p2(inp):
    return len(dfs(inp, max = 2))

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 5212
# Solution part 2: 134862

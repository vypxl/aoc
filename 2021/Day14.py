#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    lines = inp.splitlines()
    return lines[0], [(rule[0], rule[0][0] + rule[1] + rule[0][1]) for rule in structured(lines[2:], (str, None, str))]

def step2(rules, counts):
    counts_pairs = counts[0]
    ncounts_pairs = {r[0]: 0 for r in rules}
    ncounts_letters = counts[1].copy()

    for pat, rep in rules:
        if counts_pairs[pat] == 0: continue
        ncounts_letters[rep[1]] += counts_pairs[pat]
        ncounts_pairs[rep[:2]] += counts_pairs[pat]
        ncounts_pairs[rep[1:]] += counts_pairs[pat]

    return ncounts_pairs, ncounts_letters

def run(inp, n):
    poly, rules = inp
    counts_pairs = {r[0]: poly.count(r[0]) for r in rules}
    counts_letters = {c: poly.count(c) for c in flatten(counts_pairs.keys())}

    _, cs = applyN(curry(step2)(rules), n, (counts_pairs, counts_letters))
    return max(cs.values()) - min(cs.values())

def p1(inp):
    return run(inp, 10)

def p2(inp):
    return run(inp, 40)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 2068
# Solution part 2: 2158894777814

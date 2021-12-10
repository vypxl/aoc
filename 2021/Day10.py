#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

opening = "([{<"
closing = ")]}>"

points1 = {
    '(': 3,
    ')': 3,
    '[': 57,
    ']': 57,
    '{': 1197,
    '}': 1197,
    '<': 25137,
    '>': 25137,
}

comp = {
    '(': ')',
    '[': ']',
    '{': '}',
    '<': '>',
}

def check1(l):
    S = []

    for c in l:
        if c in opening:
            S.append(c)
        if c in closing:
            if comp[S.pop()] != c:
                return points1[c]
    
    return 0

def p1(inp):
    return sum([check1(line) for line in inp])

points2 = {
    '(': 1,
    '[': 2,
    '{': 3,
    '<': 4,
}

def check2(l):
    S = []

    for c in l:
        if c in opening:
            S.append(c)
        if c in closing:
            if comp[S.pop()] != c:
                return -1
                
    return reduce(lambda a, n: a * 5 + points2[n], reversed(S), 0)

def p2(inp):
    scores = sorted([x for x in [check2(line) for line in inp] if x != -1])
    return scores[len(scores) // 2]

def main():
    inp = data_lines()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 315693
# Solution part 2: 1870887234

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return np.array([[int(x) for x in l] for l in inp.splitlines()])

def findLows(inp):
    lows = []
    for i in range(len(inp)):
        for j in range(len(inp[i])):
            me = inp[i, j]
            if i > 0 and inp[i-1, j] <= me: continue
            if j > 0 and inp[i, j-1] <= me: continue
            if i < inp.shape[0] - 1 and inp[i+1, j] <= me: continue
            if j < inp.shape[1] - 1 and inp[i, j+1] <= me: continue

            lows.append((i, j))

    return lows

def p1(inp):
    return sum(1 + inp[i, j] for i, j in findLows(inp))

def countBasin(inp, i, j):
    Q = [(i, j)]
    visited = set()
    visited.add((i, j))

    while len(Q) > 0:
        i, j = Q.pop()
        for ii, jj in [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]:
            if ii < 0 or jj < 0 or ii >= inp.shape[0] or jj >= inp.shape[1]: continue
            if (ii, jj) in visited or inp[ii, jj] == 9: continue
            if inp[ii, jj] >= inp[i, j]:
                Q.append((ii, jj))
                visited.add((ii, jj))
    
    return len(visited)

def p2(inp):
    counts = [countBasin(inp, i, j) for i, j in findLows(inp)]
    return np.prod(sorted(counts)[-3:])

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 633
# Solution part 2: 1050192
# Leaderboard: 668 / 616

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

BLOCKS = [[1,1,1,1], [2,3,2], [1,1,3], [4], [2,2]]
BLOCK_INDICES = [
        lambda x, y: ([x, x+1, x+2, x+3], [y,y,y,y]),
        lambda x, y: ([x, x+1, x+1,x+1, x+2], [y+1, y, y+1, y+2,y+1]),
        lambda x, y: ([x, x+1, x+2, x+2,x+2], [y,y,y,y+1,y+2]),
        lambda x, y: ([x,x,x,x], [y, y+1, y+2, y+3]),
        lambda x, y: ([x, x, x+1, x+1], [y,y+1,y+1,y]),
        ]

def parse(inp):
    return [-1 if c == '<' else 1 for c in inp.strip()]

def p1(inp):
    heights = np.zeros((7), dtype=int)
    grid = np.zeros((7, 10000), dtype=int)

    j = 0
    m = -1
    for i in range(2022):
        which = i % 5
        block = BLOCKS[which]

        x, y = 2, m + 4

        collide = lambda x, y: (grid[BLOCK_INDICES[which](x, y)] != 0).any()

        while True:
            dir = inp[j % len(inp)]
            j += 1
            if x + dir >= 0 and x + len(block) + dir <= 7 and not collide(x + dir, y):
                x += dir
            if y > 0 and not (collide(x, y - 1)):
                y -= 1
            else:
                for i, dy in enumerate(block):
                    heights[x+i] += dy
                grid[BLOCK_INDICES[which](x, y)] = 1
                m = max(m, max(BLOCK_INDICES[which](x, y)[1]))
                break

    return m + 1

def p2(inp):
    heights = np.array([0] * 7)
    grid = np.zeros((7, 100000), dtype=int)

    correction = 0

    j = 0
    m = -1
    memo = dict()
    idx = 0
    while idx < 1000000000000:
        which = idx % 5
        block = BLOCKS[which]
        key = (which, j % len(inp), tuple(heights - min(heights)))
        if correction == 0:
            if key in memo:
                old_i, old_heights = memo[key]
                dh = (heights - np.array(old_heights))[0]
                di = idx - old_i
                correction = ((1000000000000 - idx) // di) * dh
                idx += ((1000000000000 - idx) // di) * di
            else:
                memo[key] = (idx, tuple(heights))

        x, y = 2, m + 4

        collide = lambda x, y: (grid[BLOCK_INDICES[which](x, y)] != 0).any()

        while True:
            dir = inp[j % len(inp)]
            j += 1
            if x + dir >= 0 and x + len(block) + dir <= 7 and not collide(x + dir, y):
                x += dir
            if y > 0 and not (collide(x, y - 1)):
                y -= 1
            else:
                for p in np.array(BLOCK_INDICES[which](x, y)).T:
                    heights[p[0]] = max(heights[p[0]], p[1])
                grid[BLOCK_INDICES[which](x, y)] = 1
                m = max(m, max(BLOCK_INDICES[which](x, y)[1]))
                break

        idx += 1

    m += 1 + correction
    return m

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 3048
# Solution part 2: 1504093567249

#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return [(x.split()[0], int(x.split()[1])) for x in inp.splitlines()]

def move(x, y, z, w):
        # Move only if distance is more than 1 in some direction
        if abs(x-z) > 1 or abs(y-w) > 1:
            if x > z:
                z += 1
            elif z > x:
                z -= 1

            if y > w:
                w += 1
            elif w > y:
                w -= 1

        return (x, y, z, w)

def simulate(inp, n):
    poses = set()
    rope = [(0,0)] * n

    for d, k in inp:
        for _ in range(k):
            x, y = rope[0]
            match d:
                case 'U':
                    y += 1
                case 'D':
                    y -= 1
                case 'R':
                    x += 1
                case 'L':
                    x -= 1
            rope[0] = (x, y)

            for i in range(1, n):
                x, y = rope[i-1]
                z, w = rope[i]
                x, y, z, w = move(x, y, z, w)
                rope[i-1] = (x, y)
                rope[i] = (z, w)
                if i == n-1:
                    poses.add((z,w))

    return len(list(poses))

def p1(inp):
    return simulate(inp, 2)

def p2(inp):
    return simulate(inp, 10)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 6314
# Solution part 2: 2504

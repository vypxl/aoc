#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *
from z3 import *


def parse(inp):  # noqa
    return chunks(nums(inp), 6)


def p1(inp):  # noqa
    s = 0

    lo = 200000000000000
    hi = 400000000000000
    for a, b in it.combinations(inp, 2):
        x1, y1, _, dx1, dy1, _ = a
        x2, y2, _, dx2, dy2, _ = b

        m1 = dy1 / dx1
        m2 = dy2 / dx2

        if m1 == m2:
            continue

        b1 = y1 - m1 * x1
        b2 = y2 - m2 * x2

        x = (b2 - b1) / (m1 - m2)
        y = m1 * x + b1

        dir1 = np.sign(dx1) * np.sign(x - x1)
        dir2 = np.sign(dx2) * np.sign(x - x2)

        if lo <= x <= hi and lo <= y <= hi and dir1 == dir2 == 1:
            s += 1

    return s


def p2(inp):  # noqa
    X, Y, Z, DX, DY, DZ = Int("X"), Int("Y"), Int("Z"), Int("DX"), Int("DY"), Int("DZ")

    s = Solver()
    for i, (x0, y0, z0, dx, dy, dz) in enumerate(inp):
        t = Int(f"t_{i}")
        s.add(t >= 0)
        s.add(x0 + dx * t == X + DX * t)
        s.add(y0 + dy * t == Y + DY * t)
        s.add(z0 + dz * t == Z + DZ * t)

    s.check()
    m = s.model()
    return sum(m[v].as_long() for v in [X, Y, Z])


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 15558
# Solution part 2: 765636044333842

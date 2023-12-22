#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from dataclasses import dataclass
from util import *


@dataclass
class Brick:  # noqa
    x1: int
    y1: int
    z1: int
    x2: int
    y2: int
    z2: int

    def collide_xy(self, b):  # noqa
        a = self
        return a.x1 <= b.x2 and a.x2 >= b.x1 and a.y1 <= b.y2 and a.y2 >= b.y1

    def supports(self, b):  # noqa
        return self.z2 + 1 == b.z1 and self.collide_xy(b)


def parse(inp):  # noqa
    return [Brick(*x) for x in chunks(nums(inp), 6)]


def is_supported(a, blocks):  # noqa
    if a.z1 <= 1:
        return True

    for b in blocks:
        if b is a:
            continue
        if b.supports(a):
            return True
    return False


def check_disintegrateable(a, bs):  # noqa
    others = [b for b in bs if b is not a]
    for b in others:
        if not is_supported(b, others):
            return False
    return True


def fall(blocks):  # noqa
    blocks = sorted(blocks, key=lambda x: x.z1)
    for block in blocks:
        while not is_supported(block, blocks):
            block.z1 -= 1
            block.z2 -= 1

    return blocks


def p1(inp):  # noqa
    blocks = fall(inp)

    return sum(1 for x in blocks if check_disintegrateable(x, blocks))


def p2(inp):  # noqa
    blocks = fall(inp)
    n = 0
    for i, b in enumerate(blocks):
        others = [o for o in blocks if o is not b]
        js = []
        for j, o in enumerate(others):
            if not is_supported(o, others):
                js.append(j)
                n += 1
                o.z1 -= 1
                o.z2 -= 1
        for j in js:
            others[j].z1 += 1
            others[j].z2 += 1

    return n


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 401
# Solution part 2: 63491

#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    raw = structuredre(
        inp,
        r"(U|D|L|R) (\d+) \(\#([\dabcdef]{5})(\d)\)",
        (str, int, lambda x: int(x, 16), lambda x: "RDLU"[int(x)]),
    )
    return [(x[0], x[1]) for x in raw], [(x[3], x[2]) for x in raw]


def solve(inp):  # noqa
    coords = [(0, 0)]
    x, y = 0, 0
    for dir, n_steps in inp:
        dx, dy = {
            "U": (0, -n_steps),
            "D": (0, n_steps),
            "L": (-n_steps, 0),
            "R": (n_steps, 0),
        }[dir]

        x, y = x + dx, y + dy
        coords.append((x, y))

    # Area of polygon + area outside of corners + area outside of edges
    return int(
        (
            sum(
                (xi * yj) - (xj * yi)
                for ((xi, xj), (yi, yj)) in zip(coords, coords[1:])
            )
            / 2
        )
        + len(coords)
        + sum(n - 2 for _, n in inp) / 2
    )


def main():  # noqa
    inp_1, inp_2 = parse(data())
    print(f"Solution for part 1:\n{solve(inp_1)}")
    print(f"Solution for part 2:\n{solve(inp_2)}")


if __name__ == "__main__":
    main()

# Solution part 1: 95356
# Solution part 2: 92291468914147

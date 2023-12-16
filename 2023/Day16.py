#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *

space, slash, backslash, vertical, horizontal = [0, 1, 2, 3, 4]
up, down, left, right = [(-1, 0), (1, 0), (0, -1), (0, 1)]


def parse(inp):  # noqa
    return grid(inp, "./\\|-")


def add_pos(point_1, point_2):  # noqa
    return point_1[0] + point_2[0], point_1[1] + point_2[1]


def p1(inp, start=((0, 0), right)):  # noqa
    energized = np.zeros(inp.shape, dtype=bool)

    q = Queue()
    q.put(start)
    seen = set()
    while not q.empty():
        pos, dir = q.get()
        if (pos, dir) in seen or not grid_index_valid(inp, *pos):
            continue
        seen.add((pos, dir))

        energized[*pos] = True

        tile = inp[*pos]
        if (
            tile == space
            or (tile == vertical and dir in [up, down])
            or (tile == horizontal and dir in [left, right])
        ):
            q.put((add_pos(pos, dir), dir))
        elif tile == horizontal:
            q.put((add_pos(pos, left), left))
            q.put((add_pos(pos, right), right))
        elif tile == vertical:
            q.put((add_pos(pos, up), up))
            q.put((add_pos(pos, down), down))
        elif tile == slash:
            new_dir = {
                left: down,
                right: up,
                up: right,
                down: left,
            }[dir]
            q.put((add_pos(pos, new_dir), new_dir))
        elif tile == backslash:
            new_dir = {
                left: up,
                right: down,
                up: left,
                down: right,
            }[dir]
            q.put((add_pos(pos, new_dir), new_dir))

    return energized.sum()


def p2(inp):  # noqa
    indices = (
        [((0, x), down) for x in range(inp.shape[1])]
        + [((inp.shape[1] - 1, x), up) for x in range(inp.shape[1])]
        + [((y, 0), right) for y in range(inp.shape[0])]
        + [((y, inp.shape[0] - 1), left) for y in range(inp.shape[0])]
    )
    scores = [p1(inp, start) for start in indices]
    return max(scores)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 7392
# Solution part 2: 7665

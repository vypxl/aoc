#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return inp.splitlines()


def check_part_number(inp, i, j, num_len):  # noqa
    for x in range(j - 1, j + num_len + 1):
        for y in range(i - 1, i + 2):
            if 0 <= y < len(inp) and 0 <= x < len(inp[y]):
                if inp[y][x] not in ".1234567890":
                    return True
    return False


def p1(inp):  # noqa
    s = 0
    for i, l in enumerate(inp):
        j = 0

        while j < len(l):
            if l[j].isdigit():
                k = nums(l[j:])[0]
                num_len = math.floor(math.log(k, 10)) + 1

                if check_part_number(inp, i, j, num_len):
                    s += k
                j += num_len
            else:
                j += 1

    return s


def find_number(inp, x, y):  # noqa
    if not (0 <= y <= len(inp) and 0 <= x <= len(inp[y])):
        return False, 0
    if not inp[y][x].isdigit():
        return False, 0
    while inp[y][x].isdigit():
        x -= 1
    x += 1
    return (x, y), nums(inp[y][x:])[0]


def p2(inp):  # noqa
    s = 0
    for i, l in enumerate(inp):
        j = 0

        while j < len(l):
            if l[j] == "*":
                ns = [find_number(inp, j + x, i + y) for x, y in neighbours_both]
                ks = [k for pos, k in ns if pos]
                poss = [pos for pos, k in ns if pos]
                if len(set(poss)) == 2:
                    g = np.unique(ks).prod()
                    s += g
            j += 1

    return s


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 517021
# Solution part 2: 81296995

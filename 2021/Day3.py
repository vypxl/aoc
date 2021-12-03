#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *


def p1(inp):
    g = ""
    e = ""
    count1 = [0] * len(inp[0])
    count0 = [0] * len(inp[0])
    for line in inp:
        for i, c in enumerate(line):
            if c == "0":
                count0[i] += 1
            else:
                count1[i] += 1
    for i, c in enumerate(count0):
        if count0[i] > count1[i]:
            g += "0"
            e += "1"
        else:
            g += "1"
            e += "0"

    return int(g, 2) * int(e, 2)


def most_least_common(inp, i):
    if len(inp) == 1:
        return inp[0][i], inp[0][i]

    count1 = 0
    count0 = 0
    for line in inp:
        if line[i] == "0":
            count0 += 1
        else:
            count1 += 1

    if count0 == 0:
        return "1", "1"
    elif count1 == 0:
        return "0", "0"
    if count0 > count1:
        return "0", "1"
    else:
        return "1", "0"


def p2(inp):
    a = inp
    b = inp

    i = 0
    while len(a) > 1 or len(b) > 1:
        most, _ = most_least_common(a, i)
        _, least = most_least_common(b, i)
        a = [l for l in a if l[i] == most]
        b = [l for l in b if l[i] == least]
        i += 1

    return int(a[0], 2) * int(b[0], 2)


def main():
    inp = data_lines()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 3633500
# Solution part 2: 4550283

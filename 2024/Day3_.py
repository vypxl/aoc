#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    inp = inp.replace("\n", "")
    inp = re.sub(r"do\(\)", "\n+++", inp)
    inp = re.sub(r"don't\(\)", "\n---", inp)
    inp = [(l.startswith("---"), [(int(a) * int(b)) for a, b in re.findall(r"mul\((\d+),(\d+)\)", l)]) for l in inp.splitlines()]
    return inp


def p1(inp):  # noqa
    return sum(sum(l[1]) for l in inp)


def p2(inp):  # noqa
    return sum(sum(l[1]) for l in inp if not l[0])

def main():  # noqa
    inp = parse(data())
    print(f"Part1: {p1(inp)}")
    print(f"Part2: {p2(inp)}")


if __name__ == "__main__":
    main()

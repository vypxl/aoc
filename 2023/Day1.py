#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse(inp):  # noqa
    return inp


def p1(inp):  # noqa
    digit_lines = [digits(l) for l in lines(inp)]
    calibration_values = [int(f"{xs[0]}{xs[-1]}") for xs in digit_lines]

    return sum(calibration_values)


def p2(inp):  # noqa
    adjusted = (
        inp.replace("one", "one1one")
        .replace("two", "two2two")
        .replace("three", "three3three")
        .replace("four", "four4four")
        .replace("five", "five5five")
        .replace("six", "six6six")
        .replace("seven", "seven7seven")
        .replace("eight", "eight8eight")
        .replace("nine", "nine9nine")
        .replace("zero", "zero0zero")
    )
    return p1(adjusted)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 54916
# Solution part 2: 54728
# Leaderboard: 3528 / 821

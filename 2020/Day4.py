#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    return pipe(inp.split('\n\n'),
        map(lambda x: [y for y in x.replace('\n', ' ').split(' ') if y != '']),
        list
    )

def p1(inp):
    return pipe(inp,
        map(filter(lambda x: x[:3] != 'cid')),
        filter(lambda x: len(list(x)) == 7),
        count,
    )

def p2(inp):
    pat = '^(byr:(19[2-9][0-9]|200[0-2])|iyr:(201[0-9]|2020)|eyr:(202[0-9]|2030)|hgt:((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)|hcl:#[0-9a-f]{6}|ecl:(amb|blu|brn|gry|grn|hzl|oth)|pid:[0-9]{9}|cid:.+)$'
    return pipe(inp,
        map(filter(lambda x: x[:3] != 'cid')),
        map(list),
        filter(lambda x: len(x) == 7),
        map(map(lambda x: re.match(pat, x) is not None)),
        filter(all),
        count
    )

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 196
# Solution part 2: 114

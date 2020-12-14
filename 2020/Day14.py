#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

MASK, MEM = 0, 1
bits = lambda x, n = 36: np.array(list(map(int, bin(x)[2:].zfill(n))))
unbits = lambda v: v.dot(1 << np.arange(v.shape[0] - 1, -1, -1))

def parse(inp):
    parse_mask = lambda x: np.array(list(map(int, tr(x, '01X', '120')))) - 1
    return [
        (MASK, parse_mask(xs[1])) if xs[0] == 'mask' else (MEM, int(re.findall(r'\d+', xs[0])[0]), int(xs[1]))
        for xs in [x.split(' = ') for x in inp]
    ]

def p1(inp):
    mem = {}
    mask = np.zeros(36) - 1

    for inst in inp:
        if inst[0] == MASK:
            mask = inst[1]
        else:
            mem[inst[1]] = bits(inst[2])
            mem[inst[1]][mask != -1] = mask[mask != -1]

    values = list(map(unbits, mem.values()))

    return sum(values)

def p2(inp):
    mem = {}
    mask = np.zeros(36) - 1

    for inst in inp:
        if inst[0] == MASK:
            mask = inst[1]
        else:
            addr = bits(inst[1])
            addr[mask == 1] = 1

            floating_count = mask[mask == -1].shape[0]
            for mod in np.arange(2 ** floating_count):
                a = addr.copy()
                a[mask == -1] ^= bits(mod, floating_count)
                mem[unbits(a)] = inst[2]

    return sum(mem.values())

def main():
    inp = parse(data_lines())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 11501064782628
# Solution part 2: 5142195937660

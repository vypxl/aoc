#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    inp = inp.strip()
    xs = [int(i) for i in bin(int(inp, 16))[2:]]
    return [0] * (4 * len(inp) - len(xs)) + xs

def s(x):
    return ''.join(map(str, x))
def n(x):
    return int(s(x), 2)

def readPacket(p):
    v = n(p[0:3])
    t = n(p[3:6])

    op = {
        0: lambda x, y: (0 if x is None else x) + y,
        1: lambda x, y: (1 if x is None else x) * y,
        2: lambda x, y: min((np.inf if x is None else x), y),
        3: lambda x, y: max((-np.inf if x is None else x), y),
        5: lambda x, y: y if x is None else (1 if x > y else 0),
        6: lambda x, y: y if x is None else (1 if x < y else 0),
        7: lambda x, y: y if x is None else (1 if x == y else 0),
    }

    i = 6
    if t == 4:
        ns = ''
        while(p[i] == 1):
            i += 1
            ns += s(p[i:i+4])
            i += 4
        ns += s(p[i+1:i+5])
        i += 5
        return (i, v, n(ns))
    else:
        ltid = p[i]
        i += 1
        vs = v
        ns = None
        if ltid == 0:
            tlb = n(p[i:i+15])
            i += 15
            end = i + tlb
            while i < end:
                ni, nv, nn = readPacket(p[i:])
                i += ni
                vs += nv
                ns = op[t](ns, nn)
            return (i, vs, ns)
        else:
            nsp = n(p[i:i+11])
            i += 11
            for _ in range(nsp):
                ni, nv, nn = readPacket(p[i:])
                i += ni
                vs += nv
                ns = op[t](ns, nn)
            return (i, vs, ns)

def main():
    inp = parse(data())

    _, p1, p2 = readPacket(inp)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 955
# Solution part 2: 158135423448

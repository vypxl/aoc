#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def p1(inp):
    raw = list(map(int, re.findall(r'\d+', inp)))
    time = raw[0]
    busses = np.array(raw[1:])

    distances = busses - (time % busses)
    idx = distances.argmin()

    return distances[idx] * busses[idx]

def p2(inp):
    raw = np.array(inp.splitlines()[1].split(','))
    dts = np.arange(raw.shape[0])[raw != 'x']
    busses = np.array(list(map(int, raw[raw != 'x'])))

    t = 0
    P = int(busses.prod())

    for i in range(busses.shape[0]):
        ai, pi = int(-dts[i]), int(busses[i])
        ni = P // pi
        t += ai * pow(ni, -1, pi) * ni

    return t % P

def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 2092
# Solution part 2: 702970661767766

#! /usr/bin/env python
import numpy as np
from util import data_lines_nums, lmap

DIM = 3


def pmoons(system):
    return "\n".join(
        f"pos=<x={moon[0][0]}, y={moon[0][1]}, z={moon[0][2]}>, vel=<x={moon[1][0]}, y={moon[1][1]}, z={moon[1][2]}>"
        for moon in system
    )


def energy(system):
    pot = np.abs(system[:, 0]).sum(axis=1)
    kin = np.abs(system[:, 1]).sum(axis=1)
    total = pot * kin
    return total.sum()


def step(system, amount=1):
    for _ in range(amount):
        system[:, 1] += np.sign(system[:, None, 0] - system[:, 0]).sum(axis=0)
        system[:, 0] += system[:, 1]
    return system


def p1(inp):
    return energy(step(inp.copy(), 1000))


def getCycle(system, dim):
    dimSys = system[:, :, dim]
    seen = set()
    nsteps = 0

    while 1:
        e = dimSys.tostring()
        if e in seen:
            return nsteps
        seen.add(e)

        step(dimSys)
        nsteps += 1


def p2(inp):
    system = inp.copy()
    cycles = [getCycle(system, dim) for dim in range(DIM)]

    return np.lcm.reduce(cycles)


def main():
    inp = data_lines_nums()
    # inp = [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
    # inp = [[-8, -10, 0], [5, 5, 10], [2, -7, 3], [9, -8, -3]]
    inp = lmap(list, zip(inp, [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]))
    inp = np.array(inp)

    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 8044
# Solution part 2: 362375881472136

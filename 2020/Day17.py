#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from math import floor, ceil
from util import *

def parse(part, inp):
    shape = (len(inp.splitlines()), len(inp.splitlines()[0]))
    initial = np.array(list(map(int, tr(inp, '.#', '01').replace('\n', '')))).reshape(shape)

    k = 13
    gridshape = (shape[0] * k, shape[1] * k, max(shape[0], shape[1]) * k) if part == 1 \
           else (shape[0] * k, shape[1] * k, max(shape[0], shape[1]) * k, max(shape[0], shape[1]) * k)
    grid = np.zeros(gridshape, dtype=np.uint8)

    if part == 1:
        grid[
            floor(gridshape[0] / 2) - shape[0] // 2:ceil(gridshape[0] / 2) + shape[0] // 2,
            floor(gridshape[1] / 2) - shape[1] // 2:ceil(gridshape[1] / 2) + shape[1] // 2,
            gridshape[2] // 2
        ] = initial
    else:
        grid[
            floor(gridshape[0] / 2) - shape[0] // 2:ceil(gridshape[0] / 2) + shape[0] // 2,
            floor(gridshape[1] / 2) - shape[1] // 2:ceil(gridshape[1] / 2) + shape[1] // 2,
            gridshape[2] // 2,
            gridshape[3] // 2
        ] = initial

    return grid.transpose((2,0,1)) if part == 1 else grid.transpose((2,3,0,1))

NEI1 = np.array([
    [-1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0,  0,  0, 0, 0,  0, 0, 0,  1,  1,  1,  1, 1, 1,  1, 1, 1],
    [-1, -1, -1,  0,  0,  0,  1,  1,  1, -1, -1, -1,  0, 0, 0,  1, 1, 1, -1, -1, -1,  0, 0, 0,  1, 1, 1],
    [-1,  0,  1, -1,  0,  1, -1,  0,  1, -1,  0,  1, -1, 0, 1, -1, 0, 1, -1,  0,  1, -1, 0, 1, -1, 0, 1]
])

NEI2 = np.array([
    [-1] * 27 + [0] * 27 + [1] * 27,
    ([-1] * 9 + [0] * 9 + [1] * 9) * 3,
    ([-1] * 3 + [0] * 3 + [1] * 3) * 9,
    [-1, 0, 1] * 27
])

def simulate(grid, neighbour_offsets, steps):
    gridA, gridB = grid.copy(), np.zeros(grid.shape, dtype=np.uint8)

    for _ in range(steps):
        coordinates = np.array(np.where(gridA == 1)).T
        lookAt = set()
        for i in range(coordinates.shape[0]):
            for cell in (coordinates[i][:, None] + neighbour_offsets).T:
                lookAt.add(tuple(cell))

        lookAt = np.array(list(lookAt))

        gridB = gridA.copy()
        for cell in lookAt:
            cellIdx = tuple(cell)
            cellV = gridA[cellIdx]

            neighbours = gridA[tuple(cell[:,None] + neighbour_offsets)]
            neisum = neighbours.sum() - cellV
            if cellV == 1:
                if neisum not in (2, 3):
                    gridB[cellIdx] = 0
            else:
                if neisum == 3:
                    gridB[cellIdx] = 1

        gridA, gridB = gridB, gridA

    return gridA

def p1(inp):
    return simulate(parse(1, inp), NEI1, 6).sum()

def p2(inp):
    return simulate(parse(2, inp), NEI2, 6).sum()

def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 213
# Solution part 2: 1624

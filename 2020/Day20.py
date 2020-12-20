#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from math import floor, sqrt
from util import *

unbits = lambda v: v.dot(1 << np.arange(v.shape[0] - 1, -1, -1))

ID, TOP, LEFT, BOTTOM, RIGHT, GRID = 0, 1, 2, 3, 4, 5

def parse(inp):
    raw_tiles = tr(inp, '.#', '01').split('\n\n')

    tiles = []
    for raw_tile in raw_tiles:
        if not raw_tile: continue
        lines = raw_tile.splitlines()
        tile_id = int(re.findall(r'\d+', lines[0])[0])
        grid = np.array([[int(x) for x in line] for line in lines[1:]])

        u = unbits
        f = lambda x: unbits(np.flip(x))
        top, bottom, left, right = grid[0], grid[-1], grid[:, 0], grid[:, -1]
        tiles.append((tile_id, u(top), u(left), u(bottom), u(right), grid))
        tiles.append((tile_id, u(right), f(top), u(left), f(bottom), np.rot90(grid, 1)))
        tiles.append((tile_id, f(bottom), f(right), f(top), f(left), np.rot90(grid, 2)))
        tiles.append((tile_id, f(left), u(bottom), f(right), u(top), np.rot90(grid, 3)))
        tiles.append((tile_id, f(top), u(right), f(bottom), u(left), np.fliplr(grid)))
        tiles.append((tile_id, u(left), u(top), u(right), u(bottom), np.flipud(np.rot90(grid, 1))))
        tiles.append((tile_id, u(bottom), f(left), u(top), f(right), np.fliplr(np.rot90(grid, 2))))
        tiles.append((tile_id, f(right), f(bottom), f(left), f(top), np.flipud(np.rot90(grid, 3))))

    return tiles

def neighbours(grid, size):
    i = len(grid)
    if i == 0:
        return (None, None)
    if i % size == 0:
        return (grid[i - size], None)
    if i < size:
        return (None, grid[i - 1])
    return (grid[i - size], grid[i - 1])

def arrange(grid, size, used_ids, tiles):
    if len(grid) == size * size:
        return grid
    n_top, n_left = neighbours(grid, size)

    for t in tiles:
        if t[ID] in used_ids:
            continue
        if n_top is not None and t[TOP] != n_top[BOTTOM]:
            continue
        if n_left is not None and t[LEFT] != n_left[RIGHT]:
            continue

        completed_grid = arrange(grid + [t], size, used_ids | {t[ID]}, tiles)
        if completed_grid is not None:
            return completed_grid

    return None

def p1(grid, size):
    return grid[0][ID] * grid[size-1][ID] * grid[-size][ID] * grid[-1][ID]

def p2(grid, size):
    tileSize = grid[0][GRID].shape[0] - 2
    grid = np.array([tile[GRID][1:-1,1:-1] for tile in grid]).reshape((size, size, tileSize, tileSize))
    grid = np.block([[*x] for x in grid])

    monster_indices = np.array(((0,1,1,1,1,1,1,1,1,2,2,2,2,2,2), (18, 0, 5,6, 11,12, 17,18,19, 1, 4, 7, 10, 13, 16)))

    def go(grid):
        counter = 0
        for i in range(grid.shape[0] - 2):
            for j in range(grid.shape[1] - 19):
                idxs = monster_indices + np.array([i, j])[:, None]
                idxs = tuple(tuple(x) for x in idxs)
                cells = grid[idxs]
                if (cells >= 1).all():
                    counter += 15
        return counter

    counts = [go(np.rot90(grid, n)) for n in range(4)]
    counts += [go(np.rot90(np.fliplr(grid), n)) for n in range(4)]

    return grid.sum() - sum(counts)

def main():
    inp = parse(data())
    size = floor(sqrt(len(inp) / 8))
    grid = arrange([], size, set(), inp)
    np.set_printoptions(threshold=np.inf, linewidth=np.inf)
    print(f"Solution for part 1:\n{p1(grid, size)}")
    print(f"Solution for part 2:\n{p2(grid, size)}")

if __name__ == "__main__":
    main()

# Solution part 1: 107399567124539
# Solution part 2: 1555

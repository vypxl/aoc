#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    lines = inp.splitlines()
    a = np.array(list(map(int, tr(re.sub(r"\s", '', inp), '.L#', '901')))) \
        .reshape((len(lines), len(lines[0])))
    padded = np.zeros((a.shape[0] + 2, a.shape[1] + 2), dtype=int)
    padded[:, :] = 9
    padded[1:-1, 1:-1] = a
    return padded

def step1(inp, out):
    for i in range(1, inp.shape[0] - 1):
        for j in range(1, inp.shape[1] - 1):
            if inp[i, j] == 9:
                out[i, j] = 9
                continue
            neighbours = inp[[i-1, i-1, i-1, i, i, i+1, i+1, i+1], [j-1, j, j+1, j-1, j+1, j-1, j, j+1]]
            neighboursum = neighbours[neighbours != 9].sum()
            if inp[i, j] == 0 and neighboursum == 0:
                out[i, j] = 1
            elif inp[i, j] == 1 and neighboursum >= 4:
                out[i, j] = 0
            else:
                out[i, j] = inp[i, j]

def p1(inp):
    a = inp.copy()
    b = inp.copy()

    while True:
        step1(a, b)
        if (a == b).all():
            return b[b != 9].sum()
        a, b = b, a

    return -1

def step2(inp, out):
    for i in range(1, inp.shape[0] - 1):
        for j in range(1, inp.shape[1] - 1):
            if inp[i, j] == 9:
                out[i, j] = 9
                continue

            neighbours = []
            directions = [[-1,-1],[-1,0],[-1,1],[0,-1],[0,1],[1,-1],[1,0],[1,1]]
            for _d in directions:
                d = np.array(_d)
                pos = np.array([i, j])
                while True:
                    pos += d
                    try:
                        if pos[0] < 0 or pos[1] < 0:
                            raise IndexError
                        x = inp[pos[0], pos[1]]
                        if x != 9:
                            neighbours.append(x)
                            break
                    except:
                        neighbours.append(9)
                        break

            neighbours = np.array(neighbours)
            neighboursum = neighbours[neighbours != 9].sum()
            if inp[i, j] == 0 and neighboursum == 0:
                out[i, j] = 1
            elif inp[i, j] == 1 and neighboursum >= 5:
                out[i, j] = 0
            else:
                out[i, j] = inp[i, j]

def p2(inp):
    a = inp.copy()
    b = inp.copy()

    while True:
        step2(a, b)
        if (a == b).all():
            return b[b != 9].sum()
        a, b = b, a

    return -1

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 2166
# Solution part 2: 1955

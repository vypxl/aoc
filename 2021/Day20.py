#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    alg, img = superlines(inp)
    return (grid(alg, '.#')[0], grid(img, '.#'))

def get(alg, img, x, y):
    if x == 0 or x == img.shape[0]-1 or y == 0 or y == img.shape[1]-1:
        return alg[0] if img[x, y] == 0 else alg[-1]

    w = img[x-1:x+2, y-1:y+2]
    # idx = int(''.join(str(v) for v in w.reshape((-1))), 2)
    idx = np.packbits(w).view(np.uint16)[0].byteswap() >> 7
    return alg[idx]

def solve(inp, debug=False):
    alg, oimg = inp
    ow, oh = oimg.shape
    mult = 3
    w, h = ow * mult, oh * mult

    img = np.zeros((w, h), dtype=int)
    img[ow:2*ow, oh:2*oh] = oimg

    if debug:
        print("step 0")
        printgrid(img, crop=True)

    sol1 = -1
    for i in range(50):
        imgb = np.zeros((w, h), dtype=int)
        for x in range(w):
            for y in range(h):
                imgb[x, y] = get(alg, img, x, y)
        if debug:
            printgrid(imgb, crop=True)
            print("^^^ step ", i+1)

        img = imgb
        if i == 1: sol1 = img.sum()

    return sol1, img.sum()

def main():
    inp = parse(data())
    p1, p2 = solve(inp, False)
    print(f"Solution for part 1:\n{p1}")
    print(f"Solution for part 2:\n{p2}")

if __name__ == "__main__":
    main()

# Solution part 1: 5563
# Solution part 2: 19743


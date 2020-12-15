#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

# unused
def run(nums, n_iterations):
    order = list(reversed(nums))
    spoken = set(order[1:])

    for i in range(len(nums), n_iterations):
        n = order[0]
        if n in spoken:
            dist = order[1:].index(n) + 1
            order.insert(0, dist)
        else:
            spoken.add(n)
            order.insert(0, 0)

    return order

def run_efficient(nums, n_iterations):
    distances = dict(map(lambda t: (t[1], -t[0]), enumerate(nums[:-1])))
    last = nums[-1]

    for i in range(len(nums)-1, n_iterations-1):
        dist = distances.get(last)
        distances[last] = -i
        last = 0 if dist is None else i + dist

    return last

def p1(inp):
    return run_efficient(inp, 2020)

def p2(inp):
    return run_efficient(inp, 30000000)

def main():
    inp = data_nums()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 232
# Solution part 2: 18929178

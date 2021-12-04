#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    groups = superlines(inp)
    order = nums(groups[0])
    boards = [np.array(nums(board)).reshape((5,5)) for board in groups[1:]]
    return order, boards

def won(marks):
    any_win = lambda xs: any(map(lambda x: np.sum(x) == 5, xs))
    return any_win(marks) or any_win(marks.T)

def score(board, marks, last_num):
    return int(np.sum(board * np.abs(marks - 1)) * last_num)

def p1(inp):
    order, boards = inp
    marked = np.zeros((len(boards), 5, 5))

    for x in order:
        for i, board in enumerate(boards):
            marked[i][board == x] = 1
            if won(marked[i]):
                return score(board, marked[i], x)

    return "No solution found"

def p2(inp):
    order, boards = inp
    marked = np.zeros((len(boards), 5, 5))
    out = np.zeros(len(boards))

    for x in order:
        for i, board in enumerate(boards):
            marked[i][board == x] = 1
            if won(marked[i]) and out[i] == 0:
                if np.sum(out) == len(out) - 1:
                    return score(board, marked[i], x)
                out[i] = 1

    return "No solution found"

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 10374
# Solution part 2: 24742
# Leaderboard: 724/655

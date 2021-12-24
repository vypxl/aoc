#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    pods = structuredre('\n'.join(inp.splitlines()[2:4]), r"[\s#]+([ABCD])#([ABCD])#([ABCD])#([ABCD])", (str, str, str, str))
    return (('.',) * 11,
        ((pods[1][0], pods[0][0]),
        (pods[1][1], pods[0][1]),
        (pods[1][2], pods[0][2]),
        (pods[1][3], pods[0][3])),
    )

score = { 'A': 1, 'B': 10, 'C': 100, 'D': 1000 }

# ugly but functional
def valid_moves(state, k=2):
    hall, rooms = state
    moves = []

    for i, room in enumerate(rooms):
        if room == [] or all(ord(x) - ord('A') == i for x in room): continue
        idx = i * 2 + 2

        def f(j):
            if j in [2, 4, 6, 8]: return False
            if hall[j] != '.': return True
            d = 1 + k - len(room)
            new_hall = hall[:j] + (room[-1],) + hall[j+1:]
            new_rooms = rooms[:i] + (room[:-1],) + rooms[i+1:]
            d += abs(idx - j)
            d *= score[room[-1]]
            moves.append(((new_hall, new_rooms), d))

        for j in reversed(range(0, idx)):
            if f(j): break
    
        for j in range(idx+1, len(hall)):
            if f(j): break

    for i, pod in enumerate(hall):
        if pod == '.': continue
        dest = ord(pod) - ord('A')
        if any(p != pod for p in rooms[dest]): continue
        a, b = min(i, dest * 2 + 2), max(i, dest * 2 + 2)
        d = k - len(rooms[dest])
        d += b - a
        d *= score[pod]
        if i == a: a += 1
        if all(x == '.' for x in hall[a:b]):
            newrooms = rooms[:dest] + ((pod,) + rooms[dest],) + rooms[dest+1:]
            moves.append(((hall[:i] + ('.',) + hall[i+1:], newrooms), d))

    return moves

def doneState(k=2):
    return (('.',) * 11, (('A',)*k, ('B',)*k, ('C',)*k, ('D',)*k))

def p1(inp):
    return dijkstra(inp, valid_moves)[0][doneState(2)]

def p2(inp):
    hall, rooms = inp
    rooms = (
        (rooms[0][0], 'D', 'D', rooms[0][1]),
        (rooms[1][0], 'B', 'C', rooms[1][1]),
        (rooms[2][0], 'A', 'B', rooms[2][1]),
        (rooms[3][0], 'C', 'A', rooms[3][1]),
    )
    return dijkstra((hall, rooms), lambda v: valid_moves(v, k=4))[0][doneState(4)]

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 12530
# Solution part 2: 50492

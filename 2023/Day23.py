#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *

empty, wall, right, left, up, down, crossing = 0, 1, 2, 3, 4, 5, 6


def parse(inp):  # noqa
    return grid(inp, ".#><^v")


def neighs(g, pos):  # noqa
    ns = []
    for n in neighbours_straight:
        npos = pos[0] + n[0], pos[1] + n[1]
        if (not grid_index_valid(g, *npos)) or g[npos] == wall:
            continue
        lookup = {up: (-1, 0), down: (1, 0), left: (0, -1), right: (0, 1)}
        if g[pos] in [up, down, left, right] and lookup[g[pos]] != n:
            continue
        lookup = {up: (1, 0), down: (-1, 0), left: (0, 1), right: (0, -1)}
        if g[npos] in [up, down, left, right] and lookup[g[npos]] == n:
            continue
        ns.append(npos)
    return ns


def p1(inp):  # noqa
    nodes = [(pos, neighs(inp, pos)) for pos in grid_indices(inp) if inp[pos] != wall]
    G = nx_from_node_list(nodes, True)

    S = (0, 1)
    goal = (inp.shape[0] - 1, inp.shape[1] - 2)

    return max(len(path) - 1 for path in nx.all_simple_paths(G, S, goal))


def p2(inp):  # noqa
    inp[inp == left] = empty
    inp[inp == right] = empty
    inp[inp == up] = empty
    inp[inp == down] = empty

    S = (0, 1)
    goal = (inp.shape[0] - 1, inp.shape[1] - 2)
    q = []
    q.append(S)

    G = nx.DiGraph([], directed=True, weighted=True)

    for j, i in grid_indices(inp):
        ns = neighs(inp, (j, i))
        if (j, i) in [S, goal] or len(ns) > 2 and inp[(j, i)] != wall:
            G.add_node((j, i))

    for pos in G.nodes():

        def con(a, b):
            return inp[b] != wall and (len(neighs(inp, a)) <= 2 or a is pos)

        dist, _ = dijkstra_grid(inp, pos, neighbours_straight, con)
        for other in G.nodes():
            if other is not pos and other in dist:
                G.add_edge(pos, other, weight=dist[other])

    return max(
        nx.path_weight(G, path, "weight") for path in nx.all_simple_paths(G, S, goal)
    )


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 2246
# Solution part 2: 6622
# Leaderboard: 946 / 1728

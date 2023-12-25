#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def solve(inp):  # noqa
    ls = [l.split(": ") for l in lines(inp)]
    g = nx.Graph((src, dest) for src, dests in ls for dest in dests.split())
    g.remove_edges_from(nx.minimum_edge_cut(g))
    return prod([len(x) for x in nx.connected_components(g)])


def main():  # noqa
    print(f"Solution for part 1:\n{solve(data())}")


if __name__ == "__main__":
    main()

# Solution part 1: 569904

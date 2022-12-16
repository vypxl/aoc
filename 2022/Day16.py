#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    # return structuredre(inp, r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels lead to valves (.+)", (str, int, str))
    xs = structuredre(inp, r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnel[a-z ]+([A-Z,\s]+)+", (str, int, str))
    for i, l in enumerate(lines(inp)):
        xs[i] = list(xs[i])
        if len(l.split("valves ")) == 1:
            xs[i][2] = [l.split("valve ")[1]]
        else:
            xs[i][2] = l.split("valves ")[1].split(", ")

    return xs

def p1(inp):
    # xs = list(sorted(inp, key=lambda x: x[2]))
    nodes = [(x[0], [(y, x[1]) for y in x[2]]) for x in inp]
    # real_nodes = []
    # for i in range(30):
        # for n in nodes:
    names = [x[0] for x in inp]
    # nodes = flatten([(x[0] + ' x', x[1]), (x[0] + ' o', x[1])] for x in nodes)
    print(nodes)
    print(names)
    g = nx_from_node_list(nodes, True, True)
    flow = dict()
    for name, f, _ in inp:
        flow[name] = f
    print(g['AA'])
    # nx_draw_graph(g)

    memo = dict()
    # walk(graph, k, position, opened_valves) returns the maximum flow achievable in k minutes
    def walk(g, k, p, o):
        nonlocal memo
        h = ''.join(sorted(o))
        if (k, p, h) in memo:
            return memo[(k, p, h)]

        # print(k, p)
        if k == 0: return 0
        choices = []

        # if current valve can be opened
        if p not in o and flow[p] > 0:
            choices.append(walk(g, k-1, p, o.union(set([p]))))
        # else:
        for n in g[p]:
            # print(n)
            choices.append(walk(g, k-1, n, o))

        f = 0
        for x in o:
            f += flow[x]
        res = max(c + f for c in choices)
        memo[(k, p, h)] = res
        return res
        # print(g, k, p, o)

    res = walk(g, 30, 'AA', set())

    return res

def p2(inp):
    names = [x[0] for x in inp]
    flows = [x[1] for x in inp]
    g = [[names.index(y) for y in x[2]] for x in inp]
    n_valves = len(inp)
    start = names.index('AA')

    print(flows)
    print(g)
    print(n_valves)
    print(start)

    N = 26 * 52**2 * 2**15
    memo = dict()
    # walk(graph, k, position, opened_valves) returns the maximum flow achievable in k minutes
    def walk(g, k, p1, p2, o):
        nonlocal memo
        if (k, p1, p2, o) in memo:
            return memo[(k, p1, p2, o)]
        if k == 0: return 0
        choices1, choices2 = [], []

        # if current valve can be opened
        if (not (o & (1 << p1))) and flows[p1] > 0:
            choices1.append((p1, o | (1 << p1)))
        for n in g[p1]:
            choices1.append((n, o))
        if (not (o & (1 << p2))) and flows[p2] > 0:
            choices2.append((p2, o | (1 << p2)))
        for n in g[p2]:
            choices2.append((n, o))

        res = 0
        for np1, no1 in choices1:
            for np2, no2 in choices2:
                x = walk(g, k-1, np1, np2, no1 | no2)
                if x > res:
                    res = x

        for i in range(n_valves):
            res += flows[i] * (1 if o & (1 << i) else 0)
        memo[(k, p1, p2, o)] = res

        if len(memo) % 1000000 == 0:
            print(f"{len(memo) / N * 100}% | {len(memo)} / {N}")


        return res

    res = walk(g, 26, start, start, 0)

    return res

ex = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""

def main():
    inp = parse(data())
    # inp = parse(ex)
    print(f"Solution for part 1:\n{p1(inp)}")
    # print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1862
# Solution part 2: 2422

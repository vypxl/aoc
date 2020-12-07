#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from queue import Queue
from util import *

mock = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

def parse(inp):
    pat = re.compile(r"([a-z]+ [a-z]+) bags contain (no other bags.|(?:\d [a-z]+ [a-z]+ bags?,?.?)+)")
    findings = list(map(parseTuple, pat.findall(inp)))
    return nx_from_node_list(findings, weighted=True, directed=True)

def parseTuple(t):
    items = pipe(t[1].split(','),
        map(lambda x: re.sub(r"\.|,|bags?", '', x).strip()),
        map(lambda x: None if x[0] == 'n' else (x[2:], int(x[0]))),
        list
    )
    return (t[0], items)

def p1(inp):
    G = nx.DiGraph.reverse(inp)
    descendants = nx.algorithms.dag.descendants(G, 'shiny gold')
    return len(descendants)

def p2(inp):
    q = Queue()
    counter = -1
    q.put(('shiny gold', 1))
    while not q.empty():
        s, m = q.get()
        counter += m
        es = inp.edges(s)
        ws = [inp.get_edge_data(*e)['weight'] * m for e in es]

        for e in zip(map(lambda x: x[1], es), ws):
            q.put(e)

    return counter

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 128
# Solution part 2: 20189

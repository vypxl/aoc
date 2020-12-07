#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

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
    return len(nx.algorithms.dag.descendants(G, 'shiny gold'))

def p2(inp):
    def run(node, weight):
        es = inp.edges(node)
        ws = [inp.get_edge_data(*e)['weight'] * weight for e in es]
        return weight +  sum(run(*e) for e in zip(map(lambda x: x[1], es), ws))

    return run('shiny gold', 1) - 1

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 128
# Solution part 2: 20189

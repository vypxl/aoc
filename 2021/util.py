from os.path import basename
import re
import itertools as it # pylint: disable=unused-import
import numpy as np # pylint: disable=unused-import
import networkx as nx # pylint: disable=unused-import
import matplotlib.pyplot as plt # pylint: disable=unused-import
from toolz.curried import * # pylint: disable=unused-wildcard-import
import __main__ as mainmodule

def get_day():
    return re.findall(r"\d+", basename(mainmodule.__file__))[0]

def data(day=None):
    return open(f"{day or get_day()}.in").read()

def data_lines(day=None):
    return data(day).splitlines()

def data_nums(day=None, by=None):
    return nums(data(day), by)

def data_lines_nums(day=None, by=None):
    return list(map(lambda l: nums(l, by), data_lines(day=day)))

def nums(s, by=None):
    if by is None:
        by = r"-?\d+"
    return list(map(int, re.findall(by, s)))

def structured(s, structure):
    """
    Parses each line in s into a tuple of given type structure by splitting it on whitespace.

    Usage example:
    ```
        s = 'abc 3 5\\nbcd 5 77\\n'
        structured(s, (str, int, int)) # => [('abc', 3, 5), ('bcd', 5, 77)]
    ```
    """
    return [tuple(t(y) for t, y in zip(structure, x.split())) for x in s.splitlines()]

def structuredre(s, regex, structure):
    """
    Parses each line in s into a tuple of given type structure by selecting the capture groups of the given regex.

    Usage example:
    ```
        s = 'x := 4\\ny := 6\\n'
        structuredre(s, r"(\w+) := (\d+)", (str, int)) # => [('x', 4), ('y': 6)]
    ```
    """
    return structured("\n".join(" ".join(re.match(regex, l).groups()) for l in s.splitlines()), structure)

def structuredre_cond(s, regexes, structures):
    """Same as structuredre, but with multiple possible regexes and corresponding structures"""
    def match_idx(l):
        for i, r in enumerate(regexes):
            if re.fullmatch(r, l):
                return i

    return list(map(lambda l: let(match_idx(l), lambda i: structuredre(l, regexes[i], structures[i])[0]), s.splitlines()))

def lines(s):
    return s.splitlines()

def superlines(s):
    return list(filter(lambda x: x != "", s.split("\n\n")))

def linegroups(s):
    """Splits s by two newlines and each split by single newlines"""
    return list(map(lambda g: g.splitlines(), s.split("\n\n")))

def grid(s, mapping):
    """Parses `s` into a numpy 2D array with each char mapped to its index in `mapping`"""
    return np.asarray(list(map(lambda l: list(map(lambda c: mapping.find(c), l)), s.splitlines())))

def printgrid(g, mapping):
    """Prints the inverse of `grid` (the original string)"""
    for i in range(g.shape[0]):
        for j in range(g.shape[1]):
            print(mapping[g[i, j]], end = '')
        print()

def call(f):
    return f()

def init(xs):
    return xs[:-1]

async def qCollect(q):
    xs = []
    while not q.empty():
        item = await q.get()
        xs.append(item)
    return xs

async def qFill(q, xs):
    for x in xs:
        await q.put(x)
    return q

def tr(s, a, b):
    return s.translate(str.maketrans(a, b))

def nx_from_node_list(nodes, directed=False, weighted=False):
    ctor = lambda x: nx.DiGraph(x, directed=True) if directed else nx.Graph
    if weighted:
        el = [(group[0], dest[0], { 'weight': dest[1] }) for group in nodes for dest in group[1] if dest]
    else:
        el = [(group[0], dest) for group in nodes for dest in group[1] if dest]
    return ctor(el)

def nx_draw_graph(G, weighted=False):
    pos = nx.shell_layout(G)
    if weighted:
        edge_labels = dict([((u, v, ), d['weight']) for u, v, d in G.edges(data=True)])
        nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)
    nx.draw(G, pos, arrows=True, with_labels=True, node_size=1800)
    plt.show()

def np_print_grid(grid, chars):
    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            print(chars[grid[i, j]], end='')
        print('')

lmap = compose(list, map)
attr = curry(flip(getattr)) # pylint: disable=no-value-for-parameter
mapattr = compose(map, attr)
applyN = curry(lambda f, n, x: reduce(lambda x, f: f(x), [f] * n, x))
swap = lambda t: (t[1], t[0])
flatten = lambda l: [item for sublist in l for item in sublist]
prod = reduce(lambda a, b: a * b)
fst = lambda x: x[0]
snd = lambda x: x[1]
thd = lambda x: x[2]
let = lambda x, f: f(x)

from os.path import basename
import re
import math # pylint: disable=unused-import
import itertools as it # pylint: disable=unused-import
from queue import PriorityQueue
from collections import defaultdict
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

def pnums(s):
    return nums(s, by=r"\d+")

def structured(s, structure):
    """
    Parses each line in s into a tuple of given type structure by splitting it on whitespace.
    Pass None as a type to skip a value.

    Usage example:
    ```
        s = 'abc 3 5\\nbcd 5 77\\n'
        structured(s, (str, int, int)) # => [('abc', 3, 5), ('bcd', 5, 77)]
    ```
    """
    if not isinstance(s, list): s = s.splitlines()
    return [tuple(t(y) for t, y in zip(structure, x.split()) if t is not None) for x in s]

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
    if not isinstance(s, list): s = s.splitlines()
    def match_idx(l):
        for i, r in enumerate(regexes):
            if re.fullmatch(r, l):
                return i

    return list(map(lambda l: let(match_idx(l), lambda i: structuredre(l, regexes[i], structures[i])[0]), s))

def lines(s):
    return s.splitlines()

def splittedlines(s):
    """Splits s into lines, then splits each line by python's `.split()`"""
    return [s.split() for s in s.splitlines()]

def superlines(s):
    return list(filter(lambda x: x != "", s.split("\n\n")))

def linegroups(s):
    """Splits s by two newlines and each split by single newlines"""
    return list(map(lambda g: g.splitlines(), s.split("\n\n")))

def grid(s, mapping=None):
    """
    Parses `s` into a numpy 2D array.
    If `mapping` is not given, the characters in `s` are assumed to be digits.
    If `mapping` is given, each char is mapped to its index in `mapping`
    """
    if mapping is None:
        return np.array([[int(c) for c in l] for l in lines(s)])
    else:
        return np.asarray([[mapping.find(c) for c in l] for l in lines(s)])

def grid_from_indices(indices):
    """
    Creates a grid of zeros large enough to contain all indices in the given list of tuples,
    and sets the grid at every index to 1.
    Also works with negative indices (everything is shifted so the smallest index is 0).
    """
    mnx = min(map(fst, indices))
    mny = min(map(snd, indices))
    mx = max(map(fst, indices)) + 1
    my = max(map(snd, indices)) + 1
    return np.array([[1 if (i, j) in indices else 0 for j in range(mny, my)] for i in range(mnx, mx)]).T

def grid_index_valid(grid, i, j):
    if isinstance(grid, np.ndarray):
        return i >= 0 and i < grid.shape[0] and j >= 0 and j < grid.shape[1]
    else:
        return i >= 0 and i < len(grid) and j >= 0 and j < len(grid[0])

def grid_indices(grid):
    return list(it.product(range(len(grid)), range(len(grid[0]))))

def printgrid(g, mapping = " █", crop = False):
    """Prints the output of `showgrid`"""
    print(showgrid(g, mapping, crop))

def showgrid(g, mapping = " █", crop = False):
    """
    Returns the inverse of `grid` (the original string)
    By default, outputs a ' ' for every 0 and a '█' for every 1.
    You can specify this by passing `mapping` as a string containing the characters you want to print for each number in order.
    You can turn off this mapping by passing None.
    """
    s = ""
    x0, xn, y0, yn = 0, g.shape[0], 0, g.shape[1]
    if crop:
        invert = g[0, 0] != 0 and g[-1, -1] != 0
        xs = np.argwhere(g == 0 if invert else g)
        x0, y0 = xs.min(axis=0)
        xn, yn = xs.max(axis=0) + 1
    for i in range(x0, xn):
        for j in range(y0, yn):
            s += str(g[i, j]) if mapping is None else mapping[g[i, j]]
        s += "\n"
    
    return s

def dijkstra(S, neighbours):
    """
    Returns the d and v for the dijkstra algorithm on the graph defined by the starting point S
    and the neighbours function.

    neighbours(v) should return a list of tuples (n, d) where n is a neighbour of v and d is the distance between them.
    """
    Q = PriorityQueue()
    Q.put((0, S))
    seen = set()
    d = defaultdict(lambda: np.inf)
    prev = defaultdict(None)
    d[S] = 0
    prev[S] = None

    while not Q.empty():
        vd, v = Q.get()
        seen.add(v)

        for neigh, cost in neighbours(v):
            if neigh in seen: continue
            nd = vd + cost
            if nd < d[neigh]:
                d[neigh] = nd
                prev[neigh] = v
                Q.put((nd, neigh))

    return d, prev

def dijkstra_grid(grid, S, neighbours, weights = None):
    """
    Returns d and prev for the Dijkstra algorithm on the given grid and the list of neighbours (eg neighbours_straight)
    If `weights` is not None, the weight off each edge will be the value of `weights` at position of the neighbour.
    """
    return dijkstra(S, lambda v: [
        ((v[0] + x, v[1] + y), weights[v[0] + x, v[1] + y] if weights is not None else 1)
        for x, y in neighbours
        if grid_index_valid(grid, v[0] + x, v[1] + y)
    ])

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

def np_print_grid(grid, chars = " █", crop = False):
    print(showgrid(grid, chars, crop)) # leaving the old one there for backwards compatibility

def chunks(a, k, slide=False):
    """
    Divides a into chunks of size k.
    [a] -> [[a]]

    If slide is true, the chunks will overlap:
    [a, b, c, d, e] -> [[a, b, c], [b, c, d], [c, d, e]]

    If slide is false, the chunks will be non-overlapping:
    [a, b, c, d, e, f] -> [[a, b, c], [d, e, f]]
    """
    if slide:
        return [a[i:i+k] for i in range(len(a)-k+1)]
    return [a[i:i+k] for i in range(0, len(a), k)]

def parts(a, k):
    """
    Divides a into k parts.
    [a] -> [[a]]

    The parts will be as of equal size:
    [a, b, c, d, e, f] -> [[a, b, c], [d, e, f]]
    [a, b, c, d, e, f] -> [[a, b], [c, d], [e, f]]
    [a, b, c, d, e, f] -> [[a], [b], [c], [d], [e], [f]]
    """
    if len(a) % k != 0:
        raise ValueError(f"Cannot divide into {k} parts")
    return chunks(a, len(a) // k)

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

neighbours_straight = [(-1, 0), (1, 0), (0, -1), (0, 1)]
neighbours_diag = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
neighbours_both = neighbours_straight + neighbours_diag

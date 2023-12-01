"""Provides various utilities used for solving AOC problems."""
import functools
from os.path import basename
import re
import math  # pylint: disable=unused-import
import itertools as it  # pylint: disable=unused-import
from queue import PriorityQueue, Queue
from collections import defaultdict
import numpy as np  # pylint: disable=unused-import
import networkx as nx  # pylint: disable=unused-import
import matplotlib.pyplot as plt  # pylint: disable=unused-import
import __main__ as mainmodule


def get_day():
    """Get the day (as string) of the current problem based on the filename of the main module."""
    return re.findall(r"\d+", basename(mainmodule.__file__))[0]


def data(day=None):
    """Get the input for the specified day or fallback on current day retrieved by get_day."""
    return open(f"{day or get_day()}.in", encoding="utf-8").read()


def data_lines(day=None):
    """Get the input split by lines."""
    return data(day).splitlines()


def data_nums(day=None, by=None):
    """Get numbers contained in the input as a list, by can set regex used to find numbers."""
    return nums(data(day), by)


def data_lines_nums(day=None, by=None):
    r"""
    Get numbers contained in the input split by lines as a list, by can set regex used to find numbers.

    1 2 3\n2 3 4 -> [[1,2,3], [2,3,4]]
    """
    return list(map(lambda l: nums(l, by), data_lines(day=day)))


def nums(s, by=None):
    """Extract numbers from a string. Use by to modify the regex used to extract them."""
    if by is None:
        by = r"-?\d+"
    return list(map(int, re.findall(by, s)))


def pnums(s):
    """Similar to nums, but only get positive numbers (ignores leading -)."""
    return nums(s, by=r"\d+")


def digits(s):
    """Get a list of digits that appear in the given string."""
    return [int(d) for d in s if d.isdigit()]


def structured(s, structure):
    r"""
    Parse each line in s into a tuple of given type structure by splitting it on whitespace. Pass None as a type to skip a value.

    Usage example:
    ```
        s = 'abc 3 5\nbcd 5 77\n'
        structured(s, (str, int, int)) # => [('abc', 3, 5), ('bcd', 5, 77)]
    ```
    """
    if not isinstance(s, list):
        s = s.splitlines()
    return [
        tuple(t(y) for t, y in zip(structure, x.split()) if t is not None) for x in s
    ]


def structuredre(s, regex, structure):
    r"""
    Parse each line in s into a tuple of given type structure by selecting the capture groups of the given regex.

    Usage example:
    ```
        s = 'x := 4\ny := 6\n'
        structuredre(s, r"(\w+) := (\d+)", (str, int)) # => [('x', 4), ('y': 6)]
    ```
    """
    return structured(
        "\n".join(" ".join(re.match(regex, l).groups()) for l in s.splitlines()),
        structure,
    )


def structuredre_cond(s, regexes, structures):
    """Similar to structuredre, but with multiple possible regexes and corresponding structures."""
    if not isinstance(s, list):
        s = s.splitlines()

    def match_idx(l):
        for i, r in enumerate(regexes):
            if re.fullmatch(r, l):
                return i
        raise ValueError(f"No regex matched line {l}")

    return list(
        map(
            lambda l: let(
                match_idx(l), lambda i: structuredre(l, regexes[i], structures[i])[0]
            ),
            s,
        )
    )


def lines(s):
    """Split s into lines."""
    return s.splitlines()


def splittedlines(s):
    """Split s into lines, then splits each line by python's `.split()`."""
    return [s.split() for s in s.splitlines()]


def superlines(s):
    """Split s by two newlines."""
    return list(filter(lambda x: x != "", s.split("\n\n")))


def linegroups(s):
    """Split s by two newlines and each split by single newlines."""
    return list(map(lambda g: g.splitlines(), s.split("\n\n")))


def grid(s, mapping=None):
    """
    Parse `s` into a numpy 2D array.

    If `mapping` is not given, the characters in `s` are assumed to be digits.
    If `mapping` is given, each char is mapped to its index in `mapping`
    """
    if mapping is None:
        return np.array([[int(c) for c in l] for l in lines(s)])
    return np.asarray([[mapping.find(c) for c in l] for l in lines(s)])


def grid_from_indices(indices):
    """
    Create a grid of zeros large enough to contain all indices in the given list of tuples, and sets the grid at every index to 1.

    Also works with negative indices (everything is shifted so the smallest index is 0).
    """
    mnx = min(map(fst, indices))
    mny = min(map(snd, indices))
    mx = max(map(fst, indices)) + 1
    my = max(map(snd, indices)) + 1
    return np.array(
        [
            [1 if (i, j) in indices else 0 for j in range(mny, my)]
            for i in range(mnx, mx)
        ]
    ).T


def grid_index_valid(g, i, j):
    """Check if the given i, j are valid indices for the grid g."""
    if isinstance(g, np.ndarray):
        return 0 <= i < g.shape[0] and 0 <= j < g.shape[1]
    return 0 <= i < len(g) and 0 <= j < len(g[0])


def grid_indices(g):
    """Return a list of all indice tuples in the given grid."""
    return list(it.product(range(len(g)), range(len(g[0]))))


def printgrid(g, mapping=" █", crop=False):
    """Print the output of `showgrid`."""
    print(showgrid(g, mapping, crop))


def showgrid(g, mapping=" █", crop=False):
    """
    Return the inverse of `grid` (the original string), effectively rendering it to an image.

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


def shortest_path(S, neighbours, D_cond):
    """
    Return the distance from S to the nearest vertex satisfying D_cond on the graph defined by the starting point S and the neighbours function using Dijkstra's algorithm.

    neighbours(v) should return a list of tuples (n, d) where n is a neighbour of v and d is the distance between them.
    """
    Q = PriorityQueue()
    Q.put((0, S))
    seen = set()
    d = defaultdict(lambda: np.inf)
    d[S] = 0

    while not Q.empty():
        vd, v = Q.get()
        if D_cond(v):
            return vd
        seen.add(v)

        for neigh, cost in neighbours(v):
            if neigh in seen:
                continue
            new_d = vd + cost
            if new_d < d[neigh]:
                d[neigh] = new_d
                Q.put((new_d, neigh))

    return -1


def dijkstra(S, neighbours):
    """
    Return the d and v for the dijkstra algorithm on the graph defined by the starting point S and the neighbours function.

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
        # print(vd, v)
        seen.add(v)

        for neigh, cost in neighbours(v):
            if neigh in seen:
                continue
            new_d = vd + cost
            if new_d < d[neigh]:
                d[neigh] = new_d
                prev[neigh] = v
                Q.put((new_d, neigh))

    return d, prev


def _bdfs(push, pop, empty, S, neighbours):
    push((0, S))
    seen = set()
    d = defaultdict(lambda: np.inf)
    prev = defaultdict(None)
    d[S] = 0
    prev[S] = None

    while not empty():
        vd, v = pop()
        seen.add(v)

        for neigh in neighbours(v):
            if neigh in seen:
                continue
            new_d = vd + 1
            if new_d < d[neigh]:
                d[neigh] = new_d
                prev[neigh] = v
                push((new_d, neigh))

    return d, prev


def bfs(S, neighbours):
    """
    Return the d and v for the BFS algorithm on the graph defined by the starting point S and the neighbours function.

    neighbours(v) should return a list of neighbours of v
    """
    q = Queue()
    return _bdfs(q.put, q.get, q.empty, S, neighbours)


def dfs(S, neighbours):
    """
    Return the d and v for the DFS algorithm on the graph defined by the starting point S and the neighbours function.

    neighbours(v) should return a list of neighbours of v
    """
    s = []
    return _bdfs(s.append, s.pop, lambda: len(s) == 0, S, neighbours)


def _bdfs_grid(push, pop, empty, S, g, neighbours, condition=None):
    return _bdfs(
        push,
        pop,
        empty,
        S,
        lambda v: [
            (v[0] + x, v[1] + y)
            for x, y in neighbours
            if grid_index_valid(g, v[0] + x, v[1] + y)
            and (True if condition is None else condition(v, (v[0] + x, v[1] + y)))
        ],
    )


def dfs_grid(S, g, neighbours, condition=None):
    """Similar to dijkstra_grid but using DFS."""
    s = []
    return _bdfs_grid(s.append, s.pop, lambda: len(s) == 0, S, g, neighbours, condition)


def bfs_grid(S, g, neighbours, condition=None):
    """Similar to dijkstra_grid but using BFS."""
    q = Queue()
    return _bdfs_grid(q.put, q.get, q.empty, S, g, neighbours, condition)


def dijkstra_grid(g, S, neighbours, condition=None, weights=None):
    """
    Return d and prev for the Dijkstra algorithm on the given grid and the list of neighbours (eg neighbours_straight).

    If `weights` is not None, the weight off each edge will be the value of `weights` at position of the neighbour.
    """
    return dijkstra(
        S,
        lambda v: [
            (
                (v[0] + x, v[1] + y),
                weights[v[0] + x, v[1] + y] if weights is not None else 1,
            )
            for x, y in neighbours
            if grid_index_valid(g, v[0] + x, v[1] + y)
            and (True if condition is None else condition(v, (v[0] + x, v[1] + y)))
        ],
    )


def reach(S, neighbours):
    """Return a set of vertices that can be reached from the given start point S."""
    Q = Queue()

    Q.put(S)
    seen = set()

    while not Q.empty():
        v = Q.get()
        if v in seen:
            continue
        seen.add(v)

        for neigh in neighbours(v):
            Q.put(neigh)

    return seen


def find(S, neighbours, condition):
    """Take a start point S and a condition and returns True if a vertex satisfying the condition is reachable from S."""
    s = [S]
    seen = set()

    while s:
        v = s.pop()
        if v in seen:
            continue
        seen.add(v)

        for neigh in neighbours(v):
            if condition(neigh):
                return True
            s.append(neigh)

    return False


def call(f):
    """Just call f()."""
    return f()


def init(xs):
    """Return all but the last element of xs."""
    return xs[:-1]


async def qCollect(q):
    """Collect all items from a queue into a list."""
    xs = []
    while not q.empty():
        item = await q.get()
        xs.append(item)
    return xs


async def qFill(q, xs):
    """Fill a queue with the given items."""
    for x in xs:
        await q.put(x)
    return q


def tr(s, a, b):
    """
    Translate s by replacing all characters in a with the corresponding character in b.

    tr("abc", "ac", "xy") -> "xby"
    """
    return s.translate(str.maketrans(a, b))


def nx_from_node_list(nodes, directed=False, weighted=False):
    """Create networkx graph from nodes."""
    ctor = lambda x: nx.DiGraph(x, directed=True) if directed else nx.Graph
    if weighted:
        el = [
            (group[0], dest[0], {"weight": dest[1]})
            for group in nodes
            for dest in group[1]
            if dest
        ]
    else:
        el = [(group[0], dest) for group in nodes for dest in group[1] if dest]
    return ctor(el)


def nx_draw_graph(G, weighted=False):
    """Draw a networkx graph using matplotlib."""
    pos = nx.shell_layout(G)
    if weighted:
        edge_labels = dict(
            (
                (
                    u,
                    v,
                ),
                d["weight"],
            )
            for u, v, d in G.edges(data=True)
        )
        nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)
    nx.draw(G, pos, arrows=True, with_labels=True, node_size=1800)
    plt.show()


def np_print_grid(g, chars=" █", crop=False):
    """Print the output of `showgrid`."""
    print(
        showgrid(g, chars, crop)
    )  # leaving the old one there for backwards compatibility


def chunks(a, k, slide=False):
    """
    Divide a into chunks of size k.

    [a] -> [[a]]

    If slide is true, the chunks will overlap:
    [a, b, c, d, e] -> [[a, b, c], [b, c, d], [c, d, e]]

    If slide is false, the chunks will be non-overlapping:
    [a, b, c, d, e, f] -> [[a, b, c], [d, e, f]]
    """
    if slide:
        return [a[i : i + k] for i in range(len(a) - k + 1)]
    return [a[i : i + k] for i in range(0, len(a), k)]


def parts(a, k):
    """
    Divide a into k parts.

    [a] -> [[a]]

    The parts will be as of equal size:
    [a, b, c, d, e, f] -> [[a, b, c], [d, e, f]]
    [a, b, c, d, e, f] -> [[a, b], [c, d], [e, f]]
    [a, b, c, d, e, f] -> [[a], [b], [c], [d], [e], [f]]
    """
    if len(a) % k != 0:
        raise ValueError(f"Cannot divide into {k} parts")
    return chunks(a, len(a) // k)


lmap = lambda f, xs: list(map(f, xs))
mapattr = lambda name, xs: [getattr(x, name) for x in xs]
applyN = lambda f, n, x: functools.reduce(lambda x, f: f(x), [f] * n, x)
swap = lambda t: (t[1], t[0])
flatten = lambda l: [item for sublist in l for item in sublist]
prod = lambda xs: functools.reduce(lambda a, b: a * b, xs)
fst = lambda x: x[0]
snd = lambda x: x[1]
thd = lambda x: x[2]
let = lambda x, f: f(x)

neighbours_straight = [(-1, 0), (1, 0), (0, -1), (0, 1)]
neighbours_diag = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
neighbours_both = neighbours_straight + neighbours_diag

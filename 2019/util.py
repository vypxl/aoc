from os.path import basename
import re
# pylint: disable=unused-wildcard-import
from toolz.curried import *
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
        by = r"[-\d]+"
    return list(map(int, re.findall(by, s)))

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

lmap = compose(list, map)
attr = curry(flip(getattr)) # pylint: disable=no-value-for-parameter
mapattr = compose(map, attr)

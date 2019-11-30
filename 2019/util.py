# pylint: disable=unused-wildcard-import
from toolz.curried import *
import re
import __main__ as mainmodule

def get_day():
    return re.findall(r"\d+", mainmodule.__file__)[0]

def data(day=None):
    return open(f"{day or get_day()}.in").read()

def data_lines(day=None):
    return data(day).split("\n")

def data_nums(day=None, by=None):
    return nums(data(day), by)

def data_lines_nums(day=None, by=None):
    return list(map(lambda l: nums(l, by), data_lines(day=day)))

def nums(s, by=None):
    if by == None:
        by = r"[-\d]+"
    return list(map(int, re.findall(by, s)))

def call(f):
    return f()

def init(xs):
    return xs[:-1]

attr = curry(flip(getattr)) # pylint: disable=no-value-for-parameter
mapattr = compose(map, attr)

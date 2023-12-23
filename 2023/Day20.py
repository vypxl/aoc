#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *

ff = 0
con = 1
bc = 2

t = ["low", "high"]


class FF:
    state = False
    name: str
    destinations: list

    def __init__(self, name):
        self.name = name
        self.destinations = []

    def update(self, _name, pulse):
        if not pulse:
            self.state = not self.state
            return [(self.name, d, self.state) for d in self.destinations]
        return []

    def connect_src(self, other):
        pass

    def connect_dest(self, other):
        self.destinations.append(other)


class Con:
    state: dict
    sendee: bool
    name: str
    destinations: list

    def __init__(self, name):
        self.state = {}
        self.name = name
        self.sendee = False
        self.destinations = []

    def update(self, name, pulse):
        self.state[name] = pulse

        self.sendee = not all(s for s in self.state.values())
        return [(self.name, d, self.sendee) for d in self.destinations]

    def connect_src(self, other):
        self.state[other.name] = False

    def connect_dest(self, other):
        self.destinations.append(other)


class BC:
    destinations: list
    name = "broadcaster"

    def __init__(self):
        self.destinations = []

    def update(self, _name, pulse):
        return [(self.name, d, pulse) for d in self.destinations]

    def connect_src(self, other):
        pass

    def connect_dest(self, other):
        self.destinations.append(other)


class Receiver:
    name: str
    destinations = []

    def __init__(self, name):
        self.name = name

    def update(self, _name, pulse):
        return []

    def connect_src(self, other):
        pass

    def connect_dest(self, other):
        pass


def parse(inp):  # noqa
    mods = {}
    for l in lines(inp):
        name, dests = l.split(" -> ")
        dests = dests.split(", ")
        if name.startswith("%"):
            name = name[1:]
            mods[name] = FF(name)
        elif name.startswith("&"):
            name = name[1:]
            mods[name] = Con(name)
        else:
            mods[name] = BC()
        mods[name].destinations = dests

    mods["rx"] = Receiver("rx")
    mods["output"] = Receiver("output")
    for x in mods.values():
        for d in x.destinations:
            mods[d].connect_src(x)

    return mods


def p1(inp):  # noqa
    counter = [0, 0]
    for _ in range(1000):
        q = Queue()
        q.put(("button", "broadcaster", False))

        while not q.empty():
            a, b, pulse = q.get()
            counter[pulse] += 1
            for event in inp[b].update(a, pulse):
                q.put(event)
    return prod(counter)


def p2(inp):  # noqa
    sink = [m for m in inp.values() if "rx" in m.destinations][0]
    sources = [m for m in inp.values() if sink.name in m.destinations]
    i = 0
    vals = {}
    while True:
        i += 1
        q = Queue()
        q.put(("button", "broadcaster", False))

        while not q.empty():
            a, b, pulse = q.get()
            if b == sink.name and pulse == True and a not in vals:
                vals[a] = i
                if len(vals.keys()) == len(sources):
                    return math.lcm(*vals.values())

            for event in inp[b].update(a, pulse):
                q.put(event)


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    inp = parse(data())
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1: 821985143
# Solution part 2: 240853834793347

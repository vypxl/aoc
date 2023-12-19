#! /usr/bin/env python
# noqa
# pylint: disable=unused-wildcard-import, unused-argument

from util import *


def parse_rules(s):  # noqa
    xs = s.split(",")
    rules = []
    for x in xs[:-1]:
        cond, dest = x.split(":")
        op = ">" if ">" in cond else "<"
        rating, value = cond.split(op)
        rating = "xmas".index(rating)
        value = int(value)
        rules.append((op, rating, value, dest))
    rules.append((None, None, None, xs[-1]))
    return rules


def parse(inp):  # noqa
    workflows, parts = inp.split("\n\n")
    workflows = dict(structuredre(workflows, r"(\w+)\{(.+)\}", (str, parse_rules)))

    parts = chunks(nums(parts), 4)
    return workflows, parts


def do(rule, state):  # noqa
    op, which, value, _ = rule
    if op == "<":
        return state[which] < value
    if op == ">":
        return state[which] > value
    return True


def p1(inp):  # noqa
    workflows, products = inp
    s = 0
    for p in products:
        workflow = "in"
        while workflow not in ["R", "A"]:
            for rule in workflows[workflow]:
                if do(rule, p):
                    workflow = rule[3]
                    break
        if workflow == "A":
            s += sum(p)
    return s


def select(rule, state, invert=False):  # noqa
    intersect = lambda x, y: range(max(x[0], y[0]), min(x[-1], y[-1]) + 1)

    op, which, value, _ = rule
    gt, lt = "<>" if invert else "><"
    if invert:
        if op == lt:
            xs = intersect(state[which], range(0, value + 1))
            return state[:which] + (xs,) + state[which + 1 :]
        if op == gt:
            xs = intersect(state[which], range(value, 4001))
            return state[:which] + (xs,) + state[which + 1 :]
    else:
        if op == lt:
            xs = intersect(state[which], range(value))
            return state[:which] + (xs,) + state[which + 1 :]
        if op == gt:
            xs = intersect(state[which], range(value + 1, 4001))
            return state[:which] + (xs,) + state[which + 1 :]
    return state


def p2(inp):  # noqa
    workflows, _ = inp
    workflows = list(workflows.items())

    def count(workflow, state):
        nonlocal workflows
        if workflow == "in":
            return prod([len(x) for x in state])

        su = 0
        for name, rules in workflows:
            for i, rule in enumerate(rules):
                _, _, _, dest = rule
                if dest == workflow:
                    x, m, a, s = state
                    for rule_ in rules[:i]:
                        # do not match the rules before
                        x, m, a, s = select(rule_, (x, m, a, s), True)
                    new_state = select(rule, (x, m, a, s))

                    # matche this rule
                    su += count(name, new_state)

        return su

    return count("A", [(range(1, 4001)) for _ in range(4)])


def main():  # noqa
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")


if __name__ == "__main__":
    main()

# Solution part 1:
# Solution part 2:
# Leaderboard:

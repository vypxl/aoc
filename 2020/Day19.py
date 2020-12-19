#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    raw_rules, raw_strings = tuple(inp.split('\n\n'))
    rules = {}
    for l in raw_rules.splitlines():
        name, rule = tuple(l.split(': '))
        name = int(name)
        if rule == '"a"' or rule == '"b"':
            rules[name] = rule[1]
        else:
            parts = rule.split(' | ')
            part_nums = [[int(x) for x in re.findall(r'\d+', r)] for r in parts]
            rules[name] = part_nums

    return (rules, raw_strings.splitlines())

def buildRegex(rules, tres=0):
    stringrules = {}

    def step():
        toPop = []
        for ruleId in rules:
            if rules[ruleId].__class__ == ''.__class__:
                stringrules[ruleId] = rules[ruleId]
                toPop.append(ruleId)
            else:
                for i, subrule in enumerate(rules[ruleId]):
                    for j in range(len(subrule)):
                        if subrule[j] in stringrules:
                            subrule[j] = stringrules[subrule[j]]
                    if all(x.__class__ == ''.__class__ for x in subrule):
                        rules[ruleId][i] = ''.join(subrule)
                if all(x.__class__ == ''.__class__ for x in rules[ruleId]):
                    stringrules[ruleId] = '(' + '|'.join(rules[ruleId]) + ')'
                    toPop.append(ruleId)

        for ruleId in toPop:
            rules.pop(ruleId, None)

    while len(rules) > tres:
        step()
    step() # to clear that rule 31

    if tres == 0:
        return re.compile('^' + stringrules[0] + '$')
    else:
        return rules

def p1(inp):
    rules, strings = parse(inp)
    pat = buildRegex(rules)

    return count(filter(None, [pat.match(s) for s in strings]))

def p2(inp):
    rules, strings = parse(inp)
    rules[8] = [[42], [42, 8]]
    rules[11] = [[42, 31], [42, 11, 31]]
    simplified_rules = buildRegex(rules, 3)

    rule8 = '(' + simplified_rules[8][0] + ')+'
    rule11 = (simplified_rules[11][1][0], simplified_rules[11][1][2])
    final_rules = [re.compile('^' + rule8 + rule11[0] * x + rule11[1] * x + '$') for x in range(1, 10)]

    return count(filter(lambda x: x, [any([r.match(s) is not None for r in final_rules]) for s in strings]))

def main():
    inp = data()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 111
# Solution part 2: 343

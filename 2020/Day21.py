#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    foods = []
    for line in inp:
        spl = line.split(' (')
        ings = spl[0].split(' ')
        alle = spl[1][9:-1].split(', ')
        foods.append((ings, alle))

    allergens = dict()

    for ingredients, allergen in foods:
        for a in allergen:
            if a in allergens:
                allergens[a] &= set(ingredients)
            else:
                allergens[a] = set(ingredients)

    mapping = dict()
    while len(allergens) > 0:
        unambiguos = [(x[0], list(x[1])[0]) for x in allergens.items() if len(x[1]) == 1]
        for a, f in unambiguos:
            mapping[a] = f
            allergens.pop(a)
            for k in allergens:
                if f in allergens[k]:
                    allergens[k].remove(f)

    return (foods, mapping)

def p1(inp):
    foods, mapping = inp

    allergen_ings = set(mapping.values())
    non_allergen_ings = list(filter(lambda x: x not in allergen_ings, flatten([i for i, _ in foods])))
    return len(non_allergen_ings)

def p2(inp):
    _, mapping = inp
    return ','.join(map(snd, sorted(mapping.items(), key = lambda x: x[0])))

def main():
    inp = parse(data_lines())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 2493
# Solution part 2: kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh

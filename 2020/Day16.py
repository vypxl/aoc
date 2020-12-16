#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from util import *

def parse(inp):
    parse_all_numlines = lambda lines: pipe(lines.splitlines(), map(curry(re.findall)(r"\d+")), map(lambda l: pipe(l, map(int), list)), filter(lambda l: l != []), list)
    sections = inp.split("\n\n")

    ranges = parse_all_numlines(sections[0])
    ranges = list(map(lambda l: set.union(*list(it.starmap(lambda a, b: set(range(a, b + 1)), zip(*(iter(l),) * 2)))), ranges))
    field_names = list(map(lambda l: l.split(':')[0], sections[0].splitlines()))
    my_ticket = parse_all_numlines(sections[1])[0]
    nearby_tickets = parse_all_numlines(sections[2])

    return (ranges, field_names, my_ticket, nearby_tickets)

def p1(inp):
    ranges, _, _, nearby_tickets = inp
    valid_values = list(reduce(set.union, ranges))
    flat_values = flatten(nearby_tickets)

    return sum(filter(lambda v: v not in valid_values, flat_values))

def p2(inp):
    ranges, field_names, my_ticket, nearby_tickets = inp
    valid_values = list(reduce(set.union, ranges))
    flat_values = flatten(nearby_tickets)

    invalid_values = set(filter(lambda v: v not in valid_values, flat_values))
    valid_tickets = list(filter(lambda t: all(v not in invalid_values for v in t), nearby_tickets))

    matches = dict(
        (field_names[field_id], set.intersection(*[
            set(ticket_pos for ticket_pos in range(len(ticket)) if ticket[ticket_pos] in field_range)
            for ticket in valid_tickets
        ]))
        for field_id, field_range in enumerate(ranges)
    )

    order = dict()

    while not len(matches) == 0:
        for k in matches.keys():
            if len(matches[k]) == 1:
                v = list(matches[k])[0]
                order[k] = v
                for k2 in matches.keys():
                    if v in matches[k2]:
                        matches[k2].remove(v)
                matches.pop(k, None)
                break

    departures = list(filter(lambda x: x.startswith('departure'), order))
    departure_field_ids = list(map(lambda k: order[k], departures))
    departure_values = list(map(lambda k: my_ticket[k], departure_field_ids))

    return prod(departure_values)

def main():
    inp = parse(data())
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 25916
# Solution part 2: 2564529489989

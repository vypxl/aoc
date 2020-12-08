#! /usr/bin/env python
# pylint: disable=unused-wildcard-import
from console import Console
from util import data_lines

def p1(inp):
    C = Console(inp)
    ips = set()

    while True:
        if C.ip in ips: break
        ips.add(C.ip)
        C.step()

    return C.acc

def p2(inp):
    for i in range(len(inp)):
        C = Console(inp)
        if C.program[i][0] == Console.OPCODES["jmp"]:
            C.program[i] = (Console.OPCODES["nop"], C.program[i][1])
        elif C.program[i][0] == Console.OPCODES["nop"]:
            C.program[i] = (Console.OPCODES["jmp"], C.program[i][1])

        ips = set([0])
        while C.step():
            if C.ip in ips: break
            ips.add(C.ip)
        else:
            return C.acc

    return False

def main():
    inp = data_lines()
    print(f"Solution for part 1:\n{p1(inp)}")
    print(f"Solution for part 2:\n{p2(inp)}")

if __name__ == "__main__":
    main()

# Solution part 1: 1930
# Solution part 2: 1688

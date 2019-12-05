#! /usr/bin/env python
from util import (nums, data, lmap)

opCodes = {
    1: ('add', 3),
    2: ('mul', 3),
    3: ('ld', 1),
    4: ('wrt', 1),
    5: ('jnz', 2),
    6: ('jz', 2),
    7: ('clt', 3),
    8: ('ceq', 3),
    99: ('hlt', 0)
}

def operate(ip, op, mem, inp, out, p1 = None, p2 = None, p3 = None):
    def wr(p, v):
        a, _ = p
        mem[a] = v
    def rd(p):
        av, m = p
        if m == 0:
            av = mem[av]
        return av

    if op == 1:
        wr(p3, rd(p1) + rd(p2))
    elif op == 2:
        wr(p3, rd(p1) * rd(p2))
    elif op == 3:
        wr(p1, inp.pop(0))
    elif op == 4:
        out.append(rd(p1))
    elif op == 5:
        if rd(p1) != 0:
            return rd(p2)
    elif op == 6:
        if rd(p1) == 0:
            return rd(p2)
    elif op == 7:
        if rd(p1) < rd(p2):
            wr(p3, 1)
        else:
            wr(p3, 0)
    elif op == 8:
        if rd(p1) == rd(p2):
            wr(p3, 1)
        else:
            wr(p3, 0)
    
    return ip + opCodes[op][1] + 1

def run(program, inp):
    out = []
    ip = 0
    prog = []

    while 1:
        op = program[ip]
        modes = lmap(lambda i: op // i % 10, [100, 1000, 10000])
        op = op % 100
        name, nparams = opCodes[op]
        params = list(zip(program[ip+1:ip+1+nparams], modes))

        paramsstr = lmap(lambda p: f"[{p[0]}]" if p[1] == 0 else str(p[0]), params[:nparams])
        prog.append(f"{name} {' '.join(paramsstr)}")

        if op == 99:
            break

        ip = operate(ip, op, program, inp, out, *params)

    return out, prog

def p1(inp):
    return run(nums(inp), [1])

def p2(inp):
    return run(nums(inp), [5])

def main():
    inp = data()
    p1result, p1prog = p1(inp)
    p2result, p2prog = p2(inp)

    print("Assembly Part 1:\n" + '\n'.join(p1prog) + '\n')
    print("Assembly Part 2:\n" + '\n'.join(p2prog))

    print(f"Solution for part 1:\n{p1result[-1]}")
    print(f"Solution for part 2:\n{p2result[-1]}")

if __name__ == "__main__":
    main()

# Solution part 1: 15508323
# Solution part 2: 9006327

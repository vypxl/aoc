import typing
import asyncio as aio
from util import (nums, lmap, qCollect, qFill)

class IntComputer:
    def __init__(self, program: str, stdin: aio.Queue, stdout: aio.Queue, *, name="", debug = False):
        """Create a computer that can run given program with input and output represented as asyncio.Queue 's. mame is used as a prefix for debug messages."""
        self.name = name
        self.debug = debug
        self.ip = 0
        self.stdin = stdin
        self.stdout = stdout
        self.program = nums(program)
        self.mem = self.program.copy()

    OPCODES = {
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

    def __format_param(self, p):
        """Format an instruction parameter (val, mode) as either val or [val] depending on address or immideate mode"""
        if p is None:
            return ""
        elif p[1] == 1:
            return str(p[0]).ljust(5)
        else:
            return f"[{p[0]}]".ljust(5)

    async def __operate(self, op, p1 = None, p2 = None, p3 = None):
        """Execute the given instruction with the given parameters on the current memory"""
        if self.debug:
            print(f"[{self.name}]: ${self.ip:04} {self.OPCODES[op][0].ljust(3)} {' '.join(map(self.__format_param, [p1, p2, p3]))}")
        def wr(p, v):
            a, _ = p
            self.mem[a] = v
        def rd(p):
            av, m = p
            if m == 0:
                av = self.mem[av]
            return av

        if op == 1:
            wr(p3, rd(p1) + rd(p2))
        elif op == 2:
            wr(p3, rd(p1) * rd(p2))
        elif op == 3:
            inVal = await self.stdin.get()
            wr(p1, inVal)
        elif op == 4:
            await self.stdout.put(rd(p1))
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
        elif op == 99:
            return -1
        
        return self.ip + self.OPCODES[op][1] + 1

    async def run(self):
        """Start executing instructions until a hlt instruction"""
        self.mem = self.program.copy()
        self.ip = 0

        while 1:
            op = self.mem[self.ip]
            modes = lmap(lambda i: op // i % 10, [100, 1000, 10000])
            op = op % 100
            _, nparams = self.OPCODES[op]
            params = list(zip(self.mem[self.ip+1:self.ip+1+nparams], modes))

            self.ip = await self.__operate(op, *params)
            if self.ip < 0:
                break

    @classmethod
    def run_direct(cls, program, inp, debug=False):
        """Execute a program with given input and return its output"""
        async def _run():
            stdin = aio.Queue()
            stdout = aio.Queue()

            await aio.gather(
                qFill(stdin, inp),
                cls(program, stdin, stdout, name="direct execution", debug=debug).run()
            )

            return await qCollect(stdout)
        
        if debug:
            print(f"Directly running \n{program.strip()}\nwith input\n{inp}\n")
        ret = aio.run(_run())
        if debug:
            print(f"\nResult: {ret}\n")
        return ret

    @classmethod
    def pipe(cls, programs_inputs, *, loop=False, debug=False, name="Program"):
        """Execute any number of programs simultaneously while piping the output from the previous program to the next in the list (individual inputs prepended)"""
        async def _run():
            stdin = aio.Queue()
            if loop:
                first_stdin = stdin
            stdout = aio.Queue()
            computers = []

            for i, pi in enumerate(programs_inputs):
                prog, inp = pi
                await qFill(stdin, inp)
                computers.append(cls(prog, stdin, stdout, name=f"{name} {i+1}", debug=debug).run())
                stdin = stdout
                if i < len(programs_inputs) - 1:
                    if loop and i == len(programs_inputs) - 2:
                        stdout = first_stdin
                    else:
                        stdout = aio.Queue()
            
            await aio.gather(*computers)

            return await qCollect(stdout)
        
        if debug:
            pistr = '\n'.join(map(lambda pi: f"{pi[0].strip()}\n{pi[1]}", programs_inputs))
            print(f"Piping the programs with respective initial inputs\n{pistr}\n")
        ret = aio.run(_run())
        if debug:
            print(f"\nResult: {ret}\n")
        return ret

# Used in:
#  - Day 5
#  - Day 7

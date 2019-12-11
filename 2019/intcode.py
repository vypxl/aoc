import asyncio as aio
import numpy as np
from util import nums, lmap, qCollect, qFill


class IntComputer:
    def __init__(
        self,
        program,
        *,
        inp=[],
        name="",
        readf=None,
        writef=None,
        memsize=8192,
        memdtype=object,
        debug=False,
    ):
        """Create a computer that can run given program with input and output represented as asyncio.Queue 's. mame is used as a prefix for debug messages."""
        self.name = name
        self.debug = debug
        self.ip = 0
        self.rel = 0
        self.out = []
        self.data = inp.copy()
        self.input = self.data.copy()

        self.program = np.array(nums(program), dtype=memdtype)
        self.mem = np.zeros(memsize, dtype=memdtype)

        if readf is None:
            self.readf = aio.coroutine(lambda: self.input.pop(0))
        else:
            self.readf = aio.coroutine(readf)
        if writef is None:
            self.writef = aio.coroutine(lambda x: self.out.append(x))
        else:
            self.writef = aio.coroutine(writef)

    OPCODES = {
        1: ("add", 3),
        2: ("mul", 3),
        3: ("ld", 1),
        4: ("wrt", 1),
        5: ("jnz", 2),
        6: ("jz", 2),
        7: ("clt", 3),
        8: ("ceq", 3),
        9: ("arb", 1),
        99: ("hlt", 0),
    }

    def __format_param(self, p):
        """Format an instruction parameter (val, mode) as either val or [val] depending on address or immideate mode"""
        if p is None:
            return ""
        elif p[1] == 1:
            return str(p[0]).ljust(6)
        elif p[1] == 2:
            return f"%[{p[0]}]".ljust(6)
        else:
            return f"[{p[0]}]".ljust(6)

    async def __operate(self, op, p1=None, p2=None, p3=None):
        """Execute the given instruction with the given parameters on the current memory"""
        if self.debug:
            print(
                f"[{self.name}]: %{self.rel:04} ${self.ip:04} {self.OPCODES[op][0].ljust(3)} {' '.join(map(self.__format_param, [p1, p2, p3]))}"
            )

        def wr(p, v):
            a, m = p
            if m == 0:
                self.mem[a] = v
            if m == 2:
                self.mem[self.rel + a] = v

        def rd(p):
            av, m = p
            if m == 0:
                av = self.mem[av]
            if m == 2:
                av = self.mem[self.rel + av]
            return av

        if op == 1:
            wr(p3, rd(p1) + rd(p2))
        elif op == 2:
            wr(p3, rd(p1) * rd(p2))
        elif op == 3:
            inVal = await self.readf()
            wr(p1, inVal)
        elif op == 4:
            await self.writef(rd(p1))
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
        elif op == 9:
            self.rel += rd(p1)
        elif op == 99:
            return -1

        return self.ip + self.OPCODES[op][1] + 1

    async def async_run(self):
        """Start executing instructions until a hlt instruction and return self.out"""
        self.out = []
        self.input = self.data.copy()
        self.mem.fill(0)
        self.mem[: self.program.size] = self.program
        self.ip = 0
        self.rel = 0

        while 1:
            op = self.mem[self.ip]
            modes = lmap(lambda i: op // i % 10, [100, 1000, 10000])
            op = op % 100
            _, nparams = self.OPCODES[op]
            params = list(zip(self.mem[self.ip + 1 : self.ip + 1 + nparams], modes))

            self.ip = await self.__operate(op, *params)
            if self.ip < 0:
                break

        return self.out

    def run(self):
        """execute async_run synchronously"""
        return aio.run(self.async_run())

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
                computers.append(
                    cls(
                        prog,
                        readf=lambda stdin=stdin: stdin.get(),
                        writef=lambda x, stdout=stdout: stdout.put(x),
                        name=f"{name} {i+1}",
                        debug=debug,
                    ).async_run()
                )
                stdin = stdout
                if i < len(programs_inputs) - 1:
                    if loop and i == len(programs_inputs) - 2:
                        stdout = first_stdin
                    else:
                        stdout = aio.Queue()

            await aio.gather(*computers)

            return await qCollect(stdout)

        if debug:
            pistr = "\n".join(
                map(lambda pi: f"{pi[0].strip()}\n{pi[1]}", programs_inputs)
            )
            print(f"Piping the programs with respective initial inputs\n{pistr}\n")
        ret = aio.run(_run())
        if debug:
            print(f"\nResult: {ret}\n")
        return ret


# Used in:
#  - Day 5
#  - Day 7
#  - Day 9
#  - Day 11

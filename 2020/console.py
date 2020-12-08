from util import *

class Console:

    OPCODES = {
        "nop": 0,
        "acc": 1,
        "jmp": 2,
    }

    def __init__(
        self,
        program_source,
        *,
        name="",
        debug=False,
    ):
        """Create a console that can run given program with input and output represented as asyncio.Queue 's. mame is used as a prefix for debug messages."""
        self.name = name
        self.debug = debug
        self.ip = 0
        self.acc = 0

        self.program = pipe(program_source,
            map(lambda x: x.split(' ')),
            map(lambda x: (Console.OPCODES[x[0]], int(x[1]))),
            list
        )

    def step(self):
        """Execute the given instruction with the given parameters on the current memory"""
        if self.ip >= len(self.program):
            return False

        op, param = self.program[self.ip]
        if self.debug:
            print(
                f"[{self.name}]: {op} {param}"
            )

        if op == 0:
            pass
        elif op == 1:
            self.acc += param
        elif op == 2:
            self.ip += param - 1

        self.ip += 1

        return True


# Used in:
#  - Day 8

from collections import deque
from string import ascii_lowercase

from a1.stack import Stack
from operations import OperationMode, Execution


class Calculator:
    def __init__(self) -> None:
        self.operation_mode: OperationMode = Execution(self)
        self.cmd: Stack = Stack()
        self.data: Stack = Stack()
        self.register: dict = {k: 0 for k in ascii_lowercase}
        self.register["a"] = "(Welcome)\"'\""

    def run(self) -> None:
        for token in self.register["a"]:
            self.cmd.push(token)
        while self.cmd:
            token = self.cmd.popleft()
            self.operation_mode.handle(token)


def main() -> None:
    calculator = Calculator()
    calculator.run()


if __name__ == "__main__":
    main()

from collections import deque
from string import ascii_lowercase

from operations import OperationMode, Execution


class Stack(deque):
    def push(self, __x) -> None:
        self.append(__x)

    def pop(self):
        return self.popleft()

    def peek(self):
        return self[len(self) - 1]


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
            token = self.cmd.pop()
            self.operation_mode.handle(token)


def main() -> None:
    calculator = Calculator()
    calculator.run()


if __name__ == "__main__":
    main()

from string import ascii_lowercase

from operations import OperationMode, Execution
from stack import Stack


class Calculator:
    def __init__(self) -> None:
        self.operation_mode: OperationMode = Execution(self)
        self.cmd: Stack = Stack()
        self.data: Stack = Stack()
        self.data.push(3)
        self.register: dict = {k: None for k in ascii_lowercase}

    def run(self) -> None:
        while self.cmd:
            token = self.cmd.pop()
            self.operation_mode.handle(token)
            print("DATASTACK: " + str(self.data))
            print("CMD: " + str(self.cmd))
            print("------------------------------")


def main() -> None:
    calculator = Calculator()
    calculator.register["a"] = '(Welcome)"\'@"'
    for token in calculator.register["a"][::-1]:
        calculator.cmd.push(token)
    calculator.run()


if __name__ == "__main__":
    main()

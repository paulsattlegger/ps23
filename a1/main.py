from string import ascii_lowercase

from operations import OperationMode, Execution
from stack import Stack


class Calculator:
    def __init__(self) -> None:
        self.operation_mode: OperationMode = Execution(self)
        self.cmd: Stack = Stack()
        self.data: Stack = Stack()
        self.register: dict = {k: "()" for k in ascii_lowercase}

    def run(self) -> None:
        while self.cmd:
            token = self.cmd.pop()
            self.operation_mode.handle(token)
            print("DATASTACK: " + str(self.data))
            print("CMD: " + str(self.cmd))
            print("------------------------------")


def main() -> None:
    calculator = Calculator()
    calculator.register["a"] = '(Welcome)"b@'
    calculator.register["b"] = '\'@b@'
    calculator.register["c"] = '\'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/"'
    calculator.register["d"] = '#1+()(4!4$1-2!_~3+$()4!4$2!@\\")2!@"'  # delete elemnts on stack and print them
    calculator.register["e"] = '\'2!@@#1+!@#$#2/#2/0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/#2-#2-0()(8!8$8!-2!*5!5$+7!7$7!7$7!7$1-2!_~6+$5!5$()7!7$2!@)2!@1$1$2$3!3$/2$"'
    for token in calculator.register["a"][::-1]:
        calculator.cmd.push(token)
    calculator.run()


if __name__ == "__main__":
    main()

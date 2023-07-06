from string import ascii_lowercase

from operations import OperationMode, Execution
from stack import Stack


class Calculator:
    """
    A calculator class that evaluates expressions.

    Attributes:
        operation_mode (OperationMode): A state variable that defines the operation mode of the calculator.
        cmd (Stack): A stack for storing commands.
        data (Stack): A stack for storing data.
        register (dict): A dictionary for storing string tokens as keys and associated data as values.
    """


    def __init__(self) -> None:
        """
         Initialize a new instance of the Calculator class.
         This will set the operation mode to 'Execution',
         and create new stacks for 'cmd' and 'data'.
         It will also create a register with the keys set as the lowercase English alphabet letters and the
         value of each key set to "()".
         """
        self.operation_mode: OperationMode = Execution(self)
        self.cmd: Stack = Stack()
        self.data: Stack = Stack()
        self.register: dict = {k: "()" for k in ascii_lowercase}

    def run(self) -> None:
        """
        Runs the calculator. It pops a token from the 'cmd' stack, handles it according to the operation mode,
        and then prints the 'data' stack and 'cmd' stack. It continues this process until the 'cmd' stack is empty.
        """
        while self.cmd:
            token = self.cmd.pop()
            self.operation_mode.handle(token)
            print(f"\n{'-' * 30}")
            print(f"DATASTACK:\t\t {self.data}")
            print(f"CMD:\t\t\t {self.cmd}")
            print(f"{'-' * 30}\n")

def main() -> None:
    """
    The main function of the program. It creates an instance of the Calculator class,
    sets up the register with different command sequences, and then runs the calculator.
    """
    calculator = Calculator()
    calculator.register["a"] = '(Welcome)"b@'
    calculator.register["b"] = '\'@b@'
    calculator.register["c"] = '\'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/"'  # mean
    calculator.register["d"] = '#1+()(4!4$1-2!_~3+$()4!4$2!@\\")2!@"'  # delete elemnts on stack and print them
    calculator.register[
        "e"] = '\'2!@@#1+!@#$#2/#2/0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/#2-#2-0()(' \
               '8!8$8!-2!*5!5$+7!7$7!7$7!7$1-2!_~6+$5!5$()7!7$2!@)2!@1$1$2$3!3$/2$"'  # variance

    calculator.register[
        "f"] = "\'@#(#2/#2/1+#2+-~!3!3$#-~!+2/)(#2/#-~!)4!4$2%3-~!2$2$@@#1-()(4!4$5$1-2!_~3+$()4!4$2!@)2!@1$1$1$\""  # median

    for token in calculator.register["a"][::-1]:
        calculator.cmd.push(token)
    calculator.run()


if __name__ == "__main__":
    main()

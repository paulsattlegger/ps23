import operator
from abc import ABC, abstractmethod
from string import ascii_lowercase, ascii_uppercase, digits

from stack import Stack

"""
Calculator Module

This module provides arithmetic and logical operations.
The operations are defined in a dictionary ARITHMETIC_OPERATIONS, using operator methods.

Classes:
    OperationMode (ABC): An abstract base class for operation modes in the calculator context.
    IntegerConstruction (OperationMode): Mode for building integer values.
    DecimalPlaceConstruction (OperationMode): Mode for building floating-point values.
    StringConstruction (OperationMode): Mode for building string values.
    Execution (OperationMode): Execution mode, performing various operations.
"""


ARITHMETIC_OPERATIONS = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.floordiv,
    "%": operator.mod,
    "//": operator.truediv,
}

EPSILON = 1e-9


def is_well_formed(string):
    """
    Checks if a given string has well-formed brackets, i.e., each opening bracket is matched by a closing one.

    Args:
        string (str): Input string.

    Returns:
        bool: True if the string has well-formed brackets, False otherwise.
    """

    stack = []  # Stack to store opening brackets
    for char in string:
        if char == "(":
            stack.append(char)
        elif char == ")":
            if len(stack) == 0 or stack[-1] != "(":
                return False  # Unmatched closing bracket
            stack.pop()
    return len(stack) == 0  # True if all opening brackets are matched


class OperationMode(ABC):
    """
    An abstract base class for operation modes in the calculator context.
    Each mode is responsible for handling a specific token (character) from the input.
    """

    def __init__(self, context: "Calculator") -> None:
        """
        Constructor for the OperationMode class.

        Args:
            context (Calculator): A calculator instance, used as the context for this mode.
        """
        self._context = context

    @abstractmethod
    def handle(self, token) -> None:
        """
        Abstract method for handling tokens.

        Args:
            token: The current token to be processed.
        """
        pass


class IntegerConstruction(OperationMode):
    """
    Mode for building integer values. This mode constructs integers digit by digit from the input.
    """

    def handle(self, token) -> None:
        """
        Handles an incoming token and constructs an integer value from it.

        Args:
            token: The current token to be processed.
        """
        match token:
            case d if d in digits:
                data = self._context.data.pop()
                data *= 10
                data += int(token)
                self._context.data.push(data)
            case ".":
                data = self._context.data.pop()
                self._context.data.push(float(data))
                self._context.operation_mode = DecimalPlaceConstruction(self._context)
            case _:
                self._context.operation_mode = Execution(self._context)
                self._context.operation_mode.handle(token)


class DecimalPlaceConstruction(OperationMode):
    """
    Mode for building floating-point values. This mode constructs floating-point numbers digit by digit from the input.
    """
    def __init__(self, context: "Calculator") -> None:
        super().__init__(context)
        self.m: int = -2

    def handle(self, token) -> None:
        """
        Handles an incoming token and constructs a floating-point value from it.

        Args:
            token: The current token to be processed.
        """

        match token:
            case d if d in digits:
                data = self._context.data.pop()
                data += float(token) * 10 ** (self.m + 1)
                self.m -= 1
                self._context.data.push(data)
            case ".":
                self._context.data.push(0.0)
                self._context.operation_mode = DecimalPlaceConstruction(self._context)
            case _:
                self._context.operation_mode = Execution(self._context)
                self._context.operation_mode.handle(token)


class StringConstruction(OperationMode):
    """
    Mode for building string values. This mode constructs strings character by character from the input.
    """

    def __init__(self, context: "Calculator"):
        super().__init__(context)
        self._mode: int = 1

    def handle(self, token) -> None:
        """
        Handles an incoming token and constructs a string value from it.

        Args:
            token: The current token to be processed.
        """
        match token:
            case "(":
                self._mode += 1
            case ")":
                self._mode -= 1
        if self._mode >= 1:
            data = self._context.data.pop()
            data = f"{data}{token}"
            self._context.data.push(data)
        elif self._mode == 0:
            self._context.operation_mode = Execution(self._context)


class Execution(OperationMode):
    """
    Execution mode, performing various operations like arithmetic operations, comparisons, etc.
    """
    def handle(self, token) -> None:
        """
        Handles an incoming token and performs a corresponding operation.

        Args:
            token: The current token to be processed.
        """
        match token:
            case "(":
                self._context.data.push("")
                self._context.operation_mode = StringConstruction(self._context)
            case "+" | "-" | "*" | "/" | "%":
                self.arithmetic_operator(token)
            case l if l in ascii_lowercase:
                data = self._context.register[token]
                self._context.data.push(data)
            case l if l in ascii_uppercase:
                data = self._context.data.pop()
                self._context.register[token.lower()] = data
            case d if d in digits:
                self._context.data.push(int(token))
                self._context.operation_mode = IntegerConstruction(self._context)
            case "'":
                self.read_input()
            case '"':
                self.write_output()
            case ".":
                self._context.data.push(0.0)
                self._context.operation_mode = DecimalPlaceConstruction(self._context)
            case "=" | "<" | ">":
                self.comparison_operator(token)
            case "&" | "|":
                self.logical_operator(token)
            case "_":
                self.null_check()
            case "~":
                self.negate()
            case "?":
                a = self._context.data.pop()
                if type(a) is float:
                    self._context.data.push(int(a))
                else:
                    self._context.data.push("()")
            case "!":
                self.copy()
            case "@":
                self.apply_immediately()
            case "$":
                self.delete()
            case "\\":
                self.apply_later()
            case "#":
                self._context.data.push(len(self._context.data))

    def apply_later(self) -> None:
        """
        This method retrieves a command from the data stack and schedules it for later application,
        by pushing it to the left of the command deque. It doesn't return anything.
        This operation is typically used when the execution of a command needs to be deferred.
        """
        command = self._context.data.pop()
        if type(command) is not str:
            self._context.data.push(command)
            return
        for token in command:
            self._context.cmd.appendleft(token)

    def apply_immediately(self) -> None:
        """
        This method retrieves a command from the data stack and applies it immediately,
        by pushing it to the right of the command deque. It doesn't return anything.
        This operation is typically used when a command needs to be executed immediately,
        without waiting for other pending commands.
        """
        command = self._context.data.pop()
        if type(command) is not str:
            # if it is not a string --> no effect
            self._context.data.push(command)
            return
        command = command[::-1]
        for token in command:
            self._context.cmd.push(token)

    def delete(self) -> None:
        """
        This method removes an item from the data stack at a specific position. It doesn't return anything.
        This operation is typically used when you want to discard a specific value from the stack.
        """
        n = self._context.data.pop()
        if type(n) is not int:
            self._context.data.push(n)
            return

        length: int = len(self._context.data)
        if n > length:
            self._context.data.push(n)
            return
        self._context.data[length - n] = None
        stack = self._context.data
        self._context.data = Stack()
        for i in stack:
            if i is not None:
                self._context.data.push(i)

    def copy(self) -> None:
        """
        This method copies an item from the data stack at a specific position, and pushes the copy onto the stack.
        It doesn't return anything.
        This operation is typically used when you want to duplicate a value in the stack.
        """
        a = self._context.data.pop()
        if type(a) is not int:
            return
        try:
            length = len(self._context.data) + 1
            data = self._context.data[length - a]
            self._context.data.push(data)
        except IndexError:
            print("IndexError")

    def negate(self) -> None:
        """
        This method negates the top value on the data stack (i.e., changes its sign). It doesn't return anything.
        This operation is typically used when you want to change the sign of the top value on the stack.
        """
        a = self._context.data.pop()
        if type(a) is int or type(a) is float:
            self._context.data.push(-a)
        else:
            self._context.data.push("()")

    def null_check(self) -> None:
        """
        This method checks if the top value of the data stack is null (i.e., None in Python).
        It replaces the top value of the stack with a boolean: True if the original value was null,
        False otherwise. It doesn't return anything.
        This operation is typically used when you want to check for the presence of a value on the stack.
        """

        a = self._context.data.pop()
        if a == "()":
            self._context.data.push(1)
        elif str(a).isnumeric() and -EPSILON <= a <= EPSILON:
            self._context.data.push(1)
        else:
            self._context.data.push(0)

    def logical_operator(self, token) -> None:
        """
        This method performs logical operations, as specified by the 'token' argument,
        on the top two values of the data stack. It replaces the top two values of the stack with
        the result of the operation. It doesn't return anything.

        Args:
            token: A string representing the logical operation to perform.
                   This should be one of the following: "&" (logical and), "|" (logical or).

        This operation is typically used when you want to perform logical computations on stack values.
        """

        a = self._context.data.pop()
        b = self._context.data.pop()

        if type(a) is not int or type(b) is not int:
            self._context.data.push("()")
            return

        if token == "&":
            data = a & b
            self._context.data.push(0 if data == 0 else 1)
        elif token == "|":
            data = a | b
            self._context.data.push(0 if data == 0 else 1)

    def arithmetic_operator(self, token) -> None:
        """
        This method performs arithmetic operations, as specified by the 'token' argument,
        on the top two values of the data stack. It replaces the top two values of the stack with
        the result of the operation. It doesn't return anything.

        Args:
            token: A string representing the arithmetic operation to perform.
                   This should be one of the following: "+", "-", "*", "/", "%".

        This operation is typically used when you want to perform arithmetic computations on stack values.
        """

        a = self._context.data.pop()
        b = self._context.data.pop()

        if type(a) is str or type(b) is str:
            self._context.data.push("()")
        elif token == "/" and a == 0:
            self._context.data.push("()")
        elif token == "%" and (type(a) is float or type(b) is float):
            self._context.data.push("()")
        else:
            if (type(a) is float or type(b) is float) and token == "/":
                token = "//"
            data = ARITHMETIC_OPERATIONS[token](b, a)
            self._context.data.push(data)

    def comparison_operator(self, token) -> None:
        """
        This method performs comparison operations, as specified by the 'token' argument,
        on the top two values of the data stack. It replaces the top two values of the stack with
        the result of the operation. It doesn't return anything.

        Args:
            token: A string representing the comparison operation to perform.
                   This should be one of the following: "=", "<", ">".

        This operation is typically used when you want to compare two values on the stack.
        """
        a = self._context.data.pop()
        b = self._context.data.pop()
        compare: int = self.compare(a, b)

        if token == "=":
            self._context.data.push(1 if compare == 0 else 0)
        elif token == "<":
            self._context.data.push(1 if compare < 0 else 0)
        elif token == ">":
            self._context.data.push(1 if compare > 0 else 0)

    def compare(self, a, b, epsilon=EPSILON) -> int:
        """
       This method compares two values, 'a' and 'b'. It's typically used for performing floating point comparisons with a certain level of precision.

    Args:
        a: The first value to be compared. It could be an integer, float, or other comparable types.
        b: The second value to be compared. It could be an integer, float, or other comparable types.
        epsilon: The tolerance value for floating point comparisons. Defaults to EPSILON.
                 It's a small positive number used to handle the precision of floating point comparisons.

    Returns:
        int: Returns 0 if a == b within the specified tolerance (i.e., abs(a - b) <= epsilon),
             -1 if a < b, 1 if a > b.

    This operation is typically used when you want to compare two floating-point values with a level of precision.
        """
        a = self.cast_value(a)
        b = self.cast_value(b)

        if type(a) is int and type(b) is float:
            a = float(a)
        if type(a) is float and type(b) is int:
            b = float(b)

        # floating point comparison
        if type(a) is float and type(b) is float:
            if abs(a) > 1.0 or abs(b) > 1.0:
                epsilon *= max(abs(a), abs(b))
            if abs(a - b) <= epsilon:
                return 0
            elif a < b:
                return -1
            else:
                return 1
        # integer comparison
        if type(a) is int and type(b) is int:
            if a == b:
                return 0
            elif a < b:
                return -1
            else:
                return 1

        # string comparison
        if type(a) is str and type(b) is str:
            if a == b:
                return 0
            elif a < b:
                return -1
            else:
                return 1

        # string and integer comparison (string is always greater)
        if type(a) is str and type(b) is not str:
            return 1
        if type(a) is not str and type(b) is str:
            return -1

        raise ValueError("Cannot compare undefined types")

    @staticmethod
    def cast_value(value):
        try:
            return int(value)
        except ValueError:
            try:
                return float(value)
            except ValueError:
                return value

    def read_input(self) -> None:
        line = input()
        try:
            value = int(line)
        except ValueError:
            try:
                value = float(line)
            except ValueError:
                if ("(" in line and ")" in line) and is_well_formed(line):
                    value = line
                elif "(" not in line and ")" not in line:
                    value = line
                else:
                    value = "()"
        self._context.data.push(value)

    def write_output(self) -> None:
        data = self._context.data.pop()
        print(data)

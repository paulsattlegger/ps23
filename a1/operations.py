import operator
from abc import ABC, abstractmethod
from string import ascii_lowercase, ascii_uppercase, digits

from stack import Stack

ARITHMETIC_OPERATIONS = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.floordiv,
    "%": operator.mod,
}

EPSILON = 1e-9


class OperationMode(ABC):
    def __init__(self, context: "Calculator") -> None:
        self._context = context

    @abstractmethod
    def handle(self, token) -> None:
        pass


class IntegerConstruction(OperationMode):
    def handle(self, token) -> None:
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
    def __init__(self, context: "Calculator") -> None:
        super().__init__(context)
        self.m: int = -2

    def handle(self, token) -> None:
        match token:
            case d if d in digits:
                data = self._context.data.pop()
                data += float(token) * 10 ** (self.m + 1)
                self.m -= 1
            case ".":
                self._context.data.push(0.0)
                self._context.operation_mode = DecimalPlaceConstruction(self._context)
            case _:
                self._context.operation_mode = Execution(self._context)
                self._context.operation_mode.handle(token)


class StringConstruction(OperationMode):
    def __init__(self, context: "Calculator"):
        super().__init__(context)
        self._mode: int = 1

    def handle(self, token) -> None:
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
    def handle(self, token) -> None:
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
        command = self._context.data.pop()
        if type(command) is not str:
            return
        for token in command:
            self._context.cmd.push(token)

    def apply_immediately(self) -> None:
        command = self._context.data.pop()
        if type(command) is not str:
            return
        command = command[::-1]
        for token in command:
            self._context.cmd.push(token)

    def delete(self) -> None:
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
        a = self._context.data.pop()
        if type(a) is int or type(a) is float:
            self._context.data.push(-a)
        else:
            self._context.data.push("()")

    def null_check(self) -> None:
        a = self._context.data.pop()

        if a == "()" or -EPSILON <= a <= EPSILON:
            self._context.data.push(1)
        else:
            self._context.data.push(0)

    def logical_operator(self, token) -> None:
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
        a = self._context.data.pop()
        b = self._context.data.pop()

        if type(a) is str or type(b) is str:
            self._context.data.push("()")
        elif token == "/" and b == 0:
            self._context.data.push("()")
        elif token == "%" and type(a) is float or type(b) is float:
            self._context.data.push("()")
        else:
            data = ARITHMETIC_OPERATIONS[token](b, a)
            self._context.data.push(data)

    def comparison_operator(self, token) -> None:
        a = self._context.data.pop()
        b = self._context.data.pop()
        compare: int = self.compare(a, b)

        if token == "=":
            self._context.data.push(compare == 0)
        elif token == "<":
            self._context.data.push(compare < 0)
        elif token == ">":
            self._context.data.push(compare > 0)

    def compare(self, a, b, epsilon=EPSILON) -> int:
        """
        Compare two values a and b
        :return: 0 if a == b, -1 if a < b, 1 if a > b
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
                if ("(" in line and ")" in line) and self.is_well_formed(line):
                    value = line
                elif "(" not in line and ")" not in line:
                    value = line
                else:
                    value = "()"
        self._context.data.push(value)

    def write_output(self) -> None:
        data = self._context.data.pop()
        print(data)

    def is_well_formed(self, string):
        stack = []  # Stack to store opening brackets
        for char in string:
            if char == "(":
                stack.append(char)
            elif char == ")":
                if len(stack) == 0 or stack[-1] != "(":
                    return False  # Unmatched closing bracket
                stack.pop()
        return len(stack) == 0  # True if all opening brackets are matched

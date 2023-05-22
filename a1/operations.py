import operator
from abc import ABC, abstractmethod
from string import ascii_lowercase

ARITHMETIC_OPERATIONS = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.floordiv,
    "%": operator.mod,
}


class OperationMode(ABC):
    def __init__(self, context: "Calculator") -> None:
        self._context = context

    @abstractmethod
    def handle(self, token) -> None:
        pass


class IntegerConstruction(OperationMode):
    def handle(self, token) -> None:
        pass


class DecimalPlaceConstruction(OperationMode):
    def handle(self, token) -> None:
        pass


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
                if self._mode == 0:
                    self._context.operation_mode = Execution(self._context)
        data = self._context.data.pop()
        data += token
        self._context.data.push(data)


class Execution(OperationMode):

    def handle(self, token) -> None:
        match token:
            case "(":
                self._context.data.push("(")
                self._context.operation_mode = StringConstruction(self._context)
            case "+" | "-" | "*" | "/" | "%":
                a = self._context.data.pop()
                b = self._context.data.pop()
                data = ARITHMETIC_OPERATIONS[token](a, b)
                self._context.data.push(data)
            case l if l in ascii_lowercase:
                data = self._context.register[token]
                self._context.data.push(data)
            case "'":
                self.read_input()
            case '"':
                self.write_output()

    def dot(self) -> None:
        raise NotImplemented

    def eq(self) -> None:
        raise NotImplemented

    def le(self) -> None:
        raise NotImplemented

    def ge(self) -> None:
        raise NotImplemented

    def land(self) -> None:
        raise NotImplemented

    def lor(self) -> None:
        raise NotImplemented

    def null_check(self) -> None:
        raise NotImplemented

    def negation(self) -> None:
        raise NotImplemented

    def integer_conversion(self) -> None:
        raise NotImplemented

    def copy(self) -> None:
        raise NotImplemented

    def delete(self) -> None:
        raise NotImplemented

    def apply_immediately(self) -> None:
        raise NotImplemented

    def apply_later(self) -> None:
        raise NotImplemented

    def stack_size(self) -> None:
        raise NotImplemented

    def read_input(self) -> None:
        line = input()
        try:
            value = int(line)
        except ValueError:
            try:
                value = float(line)
            except ValueError:
                if line.startswith("(") and line.endswith(")"):
                    value = line
                else:
                    value = ""
        self._context.data.push(value)

    def write_output(self) -> None:
        data = self._context.data.pop()
        print(data)

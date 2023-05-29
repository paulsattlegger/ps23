from a2.lexer import Lexer
from a2.parser import ASTBuilder, FunctionDeclaration, Expression, Apply, Integer, Pair, PredefinedFunction, Record, \
    ASTNode


class Interpreter:

    def __init__(self):
        self._file_content = None
        self._lexer = None
        self._parser = None
        self._ast = None

        self.environment = {}

    def interpret(self, path: str):
        self._file_content = self.read_file(path)
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(f"Evaluating: {self._ast})")
        self.environment = {}
        return self.eval()

    def interpret_string(self, string: str):
        self._file_content = string
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(f"Evaluating: {self._ast}")
        self.environment = {}
        return self.eval()

    def eval(self):
        self._ast.eval(self.environment)

    @staticmethod
    def read_file(path) -> str:
        with open(path, "r") as f:
            return f.read()

    @property
    def ast(self) -> ASTNode:
        return self._ast


def main():
    interpreter = Interpreter()
    print(interpreter.interpret_string("""{x=5} sub x 1"""))


if __name__ == "__main__":
    main()

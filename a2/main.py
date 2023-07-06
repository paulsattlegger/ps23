import sys

from a2.lexer import Lexer
from a2.parser import ASTBuilder, ASTNode


class Interpreter:
    """
        The interpreter class is responsible for interpreting the AST. (i.e. it calls the lexer and parser and then
        evaluates the AST).
        It is the main entry point for the interpreter.
    """

    def __init__(self) -> None:
        self._file_content: str = ""
        self._lexer = None
        self._parser = None
        self._ast = None
        self.environment = {}

    def interpret(self, path: str) -> str:
        """
        Interprets the file at the given path.
        :param path: a path to a file.
        :return: evaluated result of the file.
        """
        self._file_content = self.read_file(path)
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(f"Evaluating: {self._ast})")
        self.environment = {}
        return self.eval()

    def interpret_string(self, string: str) -> str:
        """
        Interprets the given string.
        :param string: holding the code to be interpreted.
        :return: evaluated string
        """
        self._file_content = string
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(f"Evaluating: {self._ast}")
        self.environment = {}
        return self.eval()

    def eval(self):
        result = str(self._ast.eval(self.environment))
        return result

    @staticmethod
    def read_file(path) -> str:
        with open(path, "r") as f:
            return f.read()

    @property
    def ast(self) -> ASTNode:
        return self._ast


def main():
    if len(sys.argv) != 2:
        print("Usage: python main.py <path-to-file>")
        return
    interpreter = Interpreter()
    try:
        filename = sys.argv[1]
        print(interpreter.interpret(filename))
    except FileNotFoundError:
        print(f"File {filename} not found.")


if __name__ == "__main__":
    main()

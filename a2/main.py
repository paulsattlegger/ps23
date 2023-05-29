from a2.lexer import Lexer
from a2.parser import ASTBuilder, FunctionDeclaration, Program, Apply, Integer, Pairs, PredefinedFunction


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
        return self.eval(self.ast, self.environment)

    def interpret_string(self, string: str):
        self._file_content = string
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(f"Evaluating: {self._ast})")
        self.environment = {}
        return self.eval(self.ast, self.environment)

    def eval(self, node, env):
        if isinstance(node, Program):
            return self.eval(node.expr, env)
        elif isinstance(node, FunctionDeclaration):
            return node.eval(env, [])
        elif isinstance(node, Apply):
            return node.eval(env)
        elif isinstance(node, Integer):
            return node.eval(env)
        elif isinstance(node, Pairs):
            return node.eval(env)
        elif isinstance(node, PredefinedFunction):
            return node.eval(env)
        else:
            raise Exception(f"Unknown node {node}")

    @staticmethod
    def read_file(path) -> str:
        with open(path, "r") as f:
            return f.read()

    @property
    def ast(self):
        return self._ast


def main():
    interpreter = Interpreter()
    print(interpreter.interpret("test_files/additon.txt"))


if __name__ == "__main__":
    main()

from a2.lexer import Lexer
from a2.parser import ASTBuilder, FunctionDeclaration, Program, Apply, Basic, Pairs, PredefinedFunction

PREDEFINED_FUNCS_EVAL = {
    'add': lambda x, y: x + y,
    'minus': lambda x, y: x - y,
    'mult': lambda x, y: x * y,
    'div': lambda x, y: x / y,
    'cond': lambda x, y, z: y if x else z
}


class Interpreter:

    def __init__(self, path: str):
        self._file_content = self.read_file(path)
        self._lexer = Lexer(self._file_content)
        self._parser = ASTBuilder(self._lexer)
        self._ast = self._parser.parse()
        print(self._ast)
        self.environment = {}

    def interpret(self):
        return self.eval(self.ast, self.environment)

    def eval(self, node, env):
        if isinstance(node, Program):
            return self.eval(node.expr, env)
        elif isinstance(node, FunctionDeclaration):
            return node.eval(env)
        elif isinstance(node, Apply):
            return node.eval(env)
        elif isinstance(node, Basic):
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
    interpreter = Interpreter("test_files/add_mult")
    print(interpreter.interpret())


if __name__ == "__main__":
    main()

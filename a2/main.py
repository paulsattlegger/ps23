from a2.lexer import Lexer
from a2.parser import ASTBuilder, FunctionDeclaration, Apply, Integer, Pair, PredefinedFunction, Record, \
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
    interpreter = Interpreter()
    #print(interpreter.interpret_string("""minus ((x -> y ->  add (mult x x) y) 2 5) ((x -> y -> add (mult x x) y) 2 3)"""))
    print(interpreter.interpret_string("""
{
append = x->y->cond x {head=x head, tail=append(x tail)y} y,
gen = x->cond x (append(gen(minus x 1)) {head=x, tail={}}) {}
}
gen 3
    """))


if __name__ == "__main__":
    main()

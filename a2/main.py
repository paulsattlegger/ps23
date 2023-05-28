from a2.lexer import Lexer


class Interpreter:

    def __init__(self, path: str):
        self._file_content = self.read_file(path)
        self._lexer = Lexer(self._file_content)
        for token in self._lexer.tokens():
            print(token)

    def eval(self, expr: str) -> str:
        pass

    @staticmethod
    def read_file( path) -> str:
        with open(path, "r") as f:
            return f.read()




def main():
    interpreter = Interpreter("test_files/complex_record.txt")




if __name__ == "__main__":
    main()

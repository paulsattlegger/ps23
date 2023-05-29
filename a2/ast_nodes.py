"""
AST for grammar:

<expr> ::= <apply>
          | <name> '->' <expr>

<apply> ::= <basic>
            | <apply> <basic>

<basic> ::= <integer>
            | <name>
            | '(' <expr> ')'
            | '{' [<pairs>] '}'

<pairs> ::= <name> '=' <expr>
            | <pairs> ',' <name> '=' <expr>

Mapping between those and the classes below:
<expr>         -> parse_expr()
    -> <apply>
    -> <name> '->' <expr>
        -> parse_name()
        -> eat(TokenType.ARROW)
        -> parse_expr()

<apply>        -> parse_apply()
    -> <basic>
    -> <apply> <basic>
        -> parse_apply()
        -> parse_basic()

<basic>        -> parse_basic()
    -> <integer>
        -> eat(TokenType.INTEGER)
    -> <name>
        -> parse_name()
    -> '(' <expr> ')'
        -> eat(TokenType.LPAREN)
        -> parse_expr()
        -> eat(TokenType.RPAREN)
    -> '{' [<pairs>] '}'
        -> eat(TokenType.LBRACE)
        -> parse_pairs()
        -> eat(TokenType.RBRACE)

<pairs>        -> parse_pairs()
    -> <name> '=' <expr>
        -> parse_name()
        -> eat(TokenType.EQUALS)
        -> parse_expr()
    -> <pairs> ',' <name> '=' <expr>
        -> parse_pairs()
        -> eat(TokenType.COMMA)
        -> parse_name()
        -> eat(TokenType.EQUALS)
        -> parse_expr()

<name>         -> parse_name()
    -> eat(TokenType.NAME)

<integer>      -> TokenType.INTEGER
"->"           -> TokenType.ARROW
"="            -> TokenType.EQUALS
","            -> TokenType.COMMA
"("            -> TokenType.LPAREN
")"            -> TokenType.RPAREN
"{"            -> TokenType.LBRACE
"}"            -> TokenType.RBRACE
"""


class ASTNode:
    def __str__(self):
        return ''

    def __repr__(self):
        return self.__str__()

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class Expression(ASTNode):
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return " ".join([str(expr) for expr in self.expr])

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class FunctionDeclaration(ASTNode):
    def __init__(self, param, expr):
        self.param = param
        self.expr = expr

    def __str__(self):
        return f"({str(self.param)} -> {str(self.expr)})"

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class Apply(ASTNode):
    def __init__(self, func):
        self.func = func
        self.arguments = []

    def add_argument(self, arg):
        self.arguments.append(arg)

    def __str__(self):
        return f"({str(self.func)} {' '.join([str(arg) for arg in self.arguments])})"

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class Integer(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class Name(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True


class Pair(ASTNode):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def __str__(self):
        return f"{str(self.name)} = {str(self.expr)}"


class Record(ASTNode):

    def __init__(self):
        self.pairs = []

    def add_pair(self, pair):
        self.pairs.append(pair)

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True

    def __str__(self):
        return "{" + ", ".join([str(pair) for pair in self.pairs]) + "}"


class PredefinedFunction(ASTNode):
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        if len(self.arguments) == 0:
            return str(self.name)
        return str(self.name) + " " + " ".join([str(arg) for arg in self.arguments])

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return True

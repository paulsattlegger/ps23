from a2.lexer import TokenType, Lexer

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


class Program(ASTNode):
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return str(self.expr)


class FunctionDeclaration(ASTNode):
    def __init__(self, param, expr):
        self.param = param
        self.expr = expr

    def __str__(self):
        return f"({str(self.param)} -> {str(self.expr)})"

    def eval(self, env):
        def closure(arg):
            new_env = env.copy()
            new_env[self.param.value] = arg
            return self.expr.eval(new_env)
        return closure



class Apply(ASTNode):
    def __init__(self, func, arg):
        self.func = func
        self.arg = arg

    def __str__(self):
        return f"({str(self.func)} {str(self.arg)})"

    def eval(self, env):
        func = self.func.eval(env)
        arg_val = self.arg.eval(env)
        return func(arg_val)


class Basic(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env):
        if isinstance(self.value, str):  # it's a variable name
            return env[self.value]
        else:  # it's a literal number
            return self.value


class Pairs(ASTNode):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def __str__(self):
        return f"{str(self.name)} = {str(self.expr)}"


class PredefinedFunction(ASTNode):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)

    def eval(self, env):
        if self.name == "mult":
            return lambda x: lambda y: x * y
        elif self.name == "add":
            return lambda x: lambda y: x + y
        else:
            raise Exception("Unknown function: " + self.name)


class ASTBuilder:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = lexer.get_next_token()

    def eat(self, expected_type):
        if self.current_token.type == expected_type:
            self.current_token = self.lexer.get_next_token()
        else:
            raise Exception(f"Expected token type {expected_type}, but got {self.current_token.type}")

    def parse(self):
        return self.parse_program()

    def parse_program(self):
        expr = self.parse_expr()
        return Program(expr)

    def parse_expr(self):
        if self.current_token.type == TokenType.NAME and self.lexer.peek_next_token().type == TokenType.ARROW:
            param = self.parse_name()
            self.eat(TokenType.ARROW)
            expr = self.parse_expr()
            return FunctionDeclaration(param, expr)
        return self.parse_apply()

    def parse_apply(self):
        node = self.parse_basic()
        while self.current_token.type in (TokenType.INTEGER, TokenType.NAME, TokenType.LPAREN, TokenType.LBRACE):
            arg = self.parse_basic()
            node = Apply(node, arg)
        return node

    def parse_basic(self):
        if self.current_token.type == TokenType.INTEGER:
            value = int(self.current_token.value)
            self.eat(TokenType.INTEGER)
            return Basic(value)
        elif self.current_token.type == TokenType.NAME:
            name = self.parse_name()
            return name
        elif self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            expr = self.parse_expr()
            self.eat(TokenType.RPAREN)
            return expr
        elif self.current_token.type == TokenType.LBRACE:
            self.eat(TokenType.LBRACE)
            pairs = self.parse_pairs()
            self.eat(TokenType.RBRACE)
            return pairs

    def parse_name(self):
        name = self.current_token.value
        if name in ["add", "minus", "mult", "div", "cond"]:
            self.eat(TokenType.NAME)
            return PredefinedFunction(name)
        else:
            self.eat(TokenType.NAME)
            return Basic(name)

    def parse_pairs(self):
        pairs = []
        while self.current_token.type == TokenType.NAME:
            name = self.parse_name()
            self.eat(TokenType.EQUALS)
            expr = self.parse_expr()
            pairs.append(Pairs(name, expr))
            if self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
        return pairs



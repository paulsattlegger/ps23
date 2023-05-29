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

    def __repr__(self):
        return self.__str__()

    def substitute(self, env):
        return self


class Program(ASTNode):
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return str(self.expr)

    def substitute(self, env):
        return Program(self.expr.substitute(env))


class FunctionDeclaration(ASTNode):
    def __init__(self, param, expr):
        self.param = param
        self.expr = expr

    def __str__(self):
        return f"({str(self.param)} -> {str(self.expr)})"

    def eval(self, env, args):
        if not args and self.param.eval(env) not in env:
            return self  # incomplete function call (e.g. (x -> y-> add x y) 2)
        env[self.param.value] = args[0]
        if isinstance(self.expr, FunctionDeclaration):
            return self.expr.eval(env, args[1:])
        if isinstance(self.expr, Apply):
            return self.expr.eval(env, args[1:])
        return self.expr.eval(env)

    def substitute(self, env):
        return FunctionDeclaration(self.param, self.expr.substitute(env))


class Apply(ASTNode):
    def __init__(self, func):
        self.func = func
        self.arguments = []

    def add_argument(self, arg):
        self.arguments.append(arg)

    def __str__(self):
        return f"({str(self.func)} {' '.join([str(arg) for arg in self.arguments])})"

    def parse_new_args(self, new_args):
        for arg in new_args:
            if isinstance(arg, str):
                self.arguments.append(Name(arg))
            elif isinstance(arg, int):
                self.arguments.append(Integer(arg))
            else:
                self.arguments.append(arg)

    def eval(self, env, additional_args=None):
        if additional_args:
            self.parse_new_args(additional_args)
        if isinstance(self.func, PredefinedFunction):
            return self.func.eval(env)
        elif isinstance(self.func, FunctionDeclaration):
            args = [arg.eval(env) for arg in self.arguments]
            result = self.func.eval(env, args)
            print(f"Result: {result}")
            try:
                return result.substitute(env)
            except AttributeError:  # occurs, if e.g. ( x-> add x x)2 is called, and evaluation already finished
                return result
        elif isinstance(self.func, Name):
            return env[self.func.value]
        elif isinstance(self.func, Apply):
            # needed for nested applies such as ((x -> (y -> add x y) 2) 3))
            apply = self.func
            for arg in self.arguments:
                apply.add_argument(arg)
            return apply.eval(env)

    def substitute(self, env):
        return Apply(self.func.substitute(env))


class Integer(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env):
        return self.value

    def substitute(self, env):
        return self


class Name(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env):
        return self.value

    def substitute(self, env):
        if self.value in env:
            return env[self.value]
        return self


class Pairs(ASTNode):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def __str__(self):
        return f"{str(self.name)} = {str(self.expr)}"


class PredefinedFunction(ASTNode):
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        if len(self.arguments) == 0:
            return str(self.name)
        return str(self.name) + " " + " ".join([str(arg) for arg in self.arguments])

    def eval(self, env):
        try:
            if isinstance(self.arguments[0], Integer):
                x = self.arguments[0].eval(env)
            elif isinstance(self.arguments[0], int):  # gots substituted
                x = self.arguments[0]
            else:
                x_eval = self.arguments[0].eval(env)

                if isinstance(x_eval, str):
                    x = env[x_eval]
                else:
                    x = x_eval

            if isinstance(self.arguments[1], Integer):
                y = self.arguments[1].eval(env)
            elif isinstance(self.arguments[1], int):  # gots substituted
                y = self.arguments[1]
            else:
                y_eval = self.arguments[1].eval(env)
                if isinstance(y_eval, str):
                    y = env[y_eval]
                else:
                    y = y_eval
        except KeyError:
            return self

        if self.name == "mult":
            return x * y
        elif self.name == "add":
            return x + y
        elif self.name == "minus":
            return x - y
        elif self.name == "div":
            return x / y
        elif self.name == "cond":
            if isinstance(self.arguments[2], Integer):
                z = self.arguments[2].eval(env)
            elif isinstance(self.arguments[2], int):  # gots subsituted
                z = self.arguments[2]
            else:
                z_eval = self.arguments[2].eval(env)
                if isinstance(z_eval, str):
                    z = env[z_eval]
                else:
                    z = z_eval
            if x:
                return y
            else:
                return z
        else:
            raise Exception("Unknown function: " + self.name)

    def substitute(self, env):
        subs_args = [arg.substitute(env) for arg in self.arguments]
        # lazy evaluation
        re = PredefinedFunction(self.name, subs_args).eval(env)
        return re


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
        apply = Apply(node)
        while self.current_token.type in (TokenType.INTEGER, TokenType.NAME, TokenType.LPAREN, TokenType.LBRACE):
            arg = self.parse_basic()
            apply.add_argument(arg)
        return apply

    def parse_basic(self):
        if self.current_token.type == TokenType.INTEGER:
            value = int(self.current_token.value)
            self.eat(TokenType.INTEGER)
            return Integer(value)
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
            args = []
            if name != "cond":
                args.append(self.parse_basic())
                args.append(self.parse_basic())
            else:
                args.append(self.parse_basic())
                args.append(self.parse_basic())
                args.append(self.parse_basic())
            return PredefinedFunction(name, args)
        else:
            self.eat(TokenType.NAME)
            return Name(name)

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

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
    -> basically parses basics until there are no more basics to parse, and then returns an Apply object with the other basics
    as arguments, since an <apply> is basically just a list of basics, where the first basic is the function/recorrds/name
    and the rest are arguments

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
import copy
from typing import Dict


class ASTNode:
    def __str__(self):
        return ''

    def __repr__(self):
        return self.__str__()

    def eval(self, env: Dict[str, "ASTNode"]):
        raise NotImplementedError("eval() not implemented for abstract ASTNode")

    def condition(self, env):
        raise NotImplementedError("condition() not implemented for abstract ASTNode")


class FunctionDeclaration(ASTNode):
    def __init__(self, param, expr):
        self.param = param
        self.expr = expr
        self.args = []

    def set_args(self, args: list["ASTNode"]):
        self.args = args

    def __str__(self):
        return f"({str(self.param)} -> {str(self.expr)})"

    def eval(self, env: Dict[str, "ASTNode"]):
        """
        Functions just binds its parameter to the argument given(e.g. sets it in the environment),
        and then evaluates the body of the function
        If there is no argument given
        :param env:
        :return:
        """

        updated_env = copy.deepcopy(env)
        if len(self.args) > 0:
            if type(self.args[0]) == Record:
                arg = self.args[0].eval_pairs(env)
            else:
                arg = self.args[0].eval(env)

            updated_env[self.param.value] = arg
            self.args = self.args[1:]
            # function has multiple arguments, i.e. higher order function
            if type(self.expr) == FunctionDeclaration and len(self.args) > 0:
                self.expr.set_args(self.args)
            if type(self.expr) == Apply and len(self.args) > 0:
                for arg in self.args:
                    self.expr.add_argument(arg)
            body_evaluated = self.expr.eval(updated_env)

            if type(body_evaluated) == Record:
                """result of function is record, evaluate it"""
                body_evaluated = body_evaluated.eval_pairs(updated_env)
                return body_evaluated

            return body_evaluated

        # no argument given, return function as is
        body = self.expr.eval(updated_env)
        return FunctionDeclaration(self.param, body)

    def condition(self, env):
        return self.expr.eval(env).condition(env)


class Apply(ASTNode):
    def __init__(self, func):
        self.func = func
        self.arguments = []

    def add_argument(self, arg):
        self.arguments.append(arg)

    def __str__(self):
        return f"({str(self.func)} {' '.join([str(arg) for arg in self.arguments])})"

    def eval(self, env: Dict[str, "ASTNode"]):
        if isinstance(self.func, FunctionDeclaration):
            self.func.set_args(self.arguments)
        if isinstance(self.func, Apply):

            apply = Apply(self.func.func)
            # unwrap nested apply, e.g. (a 2)3) -> (a 2 3)
            for arg in self.func.arguments:
                apply.add_argument(arg)

            for arg in self.arguments:
                apply.add_argument(arg)

            return apply.eval(env)
        if isinstance(self.func, Record) and len(self.arguments) > 0:
            """
            A record followed by an expression is a environment update
            """
            record = self.func.eval(env)
            if not self.arguments:
                return record
            self.func = self.arguments[0]
            self.arguments = self.arguments[1:]

        if isinstance(self.func, Name):
            self.func = self.func.eval(env)
            if not isinstance(self.func, Name):
                return self.eval(env)

        result = self.func.eval(env)

        return result.eval(env)

    def condition(self, env):
        return self.eval(env).condition(env)


class Integer(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return self.value != 0


class Name(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        if self.value in env:
            return env[self.value]
        else:
            return self

    def condition(self, env):
        if self.value in env:
            return env[self.value].condition(env)


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

    def eval(self, env):
        for pair in self.pairs:
            env[pair.name.value] = pair.expr
        return self

    def eval_pairs(self, env):
        for pair in self.pairs:
            pair.expr = pair.expr.eval(env)
            env[pair.name.value] = pair.expr
        return self

    def condition(self, env):
        return self.pairs != []

    def __str__(self):
        return "{" + ", ".join([str(pair) for pair in self.pairs]) + "}"


class PredefinedFunction(ASTNode):
    functions = {
        "add": lambda x, y: x + y,
        "plus": lambda x, y: x + y,
        "minus": lambda x, y: x - y,
        "mult": lambda x, y: x * y,
        "div": lambda x, y: x / y,
        "sub": lambda x, y: x - y,
        "cond": lambda x, y, z: y if x else z,
    }

    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        if len(self.arguments) == 0:
            return str(self.name)
        return str(self.name) + " " + " ".join([str(arg) for arg in self.arguments])

    def eval(self, env: Dict[str, "ASTNode"]):
        updated_env = copy.deepcopy(env)

        if len(self.arguments) == 2:
            x = self.arguments[0].eval(updated_env)
            y = self.arguments[1].eval(updated_env)

            if type(x) == Integer and type(y) == Integer:
                return Integer(self.functions[self.name](x.value, y.value))

            # if types are not integers (i.e. no full evaluate function), return function with arguments evaluated
            return PredefinedFunction(self.name, [x, y])

        elif len(self.arguments) == 3:
            condition = self.arguments[0].condition(updated_env)
            if condition:
                return self.arguments[1].eval(updated_env)
            else:
                return self.arguments[2].eval(updated_env)
        else:
            raise Exception(f"Wrong number of arguments for {self.name}")

    def condition(self, env):
        return self.eval(env).condition(env)

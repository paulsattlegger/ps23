import copy
from typing import Dict


class ASTNode:
    """
    Abstract base class for all AST nodes.
    """

    def __str__(self) -> str:
        return ''

    def __repr__(self) -> str:
        return self.__str__()

    def eval(self, env: Dict[str, "ASTNode"]) -> "ASTNode":
        """
        Evaluates the AST node in the given environment.
        This method should be implemented by subclasses.
        """
        raise NotImplementedError("eval() not implemented for abstract ASTNode")

    def condition(self, env) -> bool:
        """
         Checks the condition of the AST node in the given environment.
         This method should be implemented by subclasses.
         """
        raise NotImplementedError("condition() not implemented for abstract ASTNode")


class FunctionDeclaration(ASTNode):
    """
    Represents a function declaration in the AST.
    e.g., "x -> plus x 1"
    """

    def __init__(self, param, expr):
        """
        :param param: the parameter name of the function
        :param expr: the body of the function
        e.g. x -> plus x 1
        param: x, body: plus x 1
        """
        self.param = param
        self.expr = expr
        self.args = []

    def set_args(self, args: list["ASTNode"]) -> None:
        """
        Sets the arguments for the function declaration.
        """
        self.args = args

    def __str__(self):
        return f"({str(self.param)} -> {str(self.expr)})"

    def eval(self, env: Dict[str, "ASTNode"]) -> ASTNode:
        """
        Evaluates the function declaration in the given environment.
        Functions bind their parameter to the argument given (e.g., sets it in the environment),
        and then evaluate the body of the function.
        If there is no argument given, it returns the function as is.
        Higher order functions are also supported, however each FunctionDeclaration stands for a function with a single argument.
        If a function can not be fully resolved (e.g. it has a parameter but no argument is given), it returns the function as is.
        """

        updated_env = copy.deepcopy(env)
        if len(self.args) > 0:
            if type(self.args[0]) == Record:
                # if argument is record, evaluate it pairs
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
                # result of function is record, evaluate it
                body_evaluated = body_evaluated.eval_pairs(updated_env)
                return body_evaluated

            return body_evaluated

        # no argument given, return function as is
        body = self.expr.eval(updated_env)
        return FunctionDeclaration(self.param, body)

    def condition(self, env) -> bool:
        """
        Function is true, IFF its evaluation is true
        """
        return self.expr.eval(env).condition(env)


class Apply(ASTNode):
    """
    Represents an application of a expression in the AST.
    """

    def __init__(self, func):
        self.func = func
        self.arguments = []

    def add_argument(self, arg):
        self.arguments.append(arg)

    def __str__(self):
        return f"({str(self.func)} {' '.join([str(arg) for arg in self.arguments])})"

    def eval(self, env: Dict[str, "ASTNode"]) -> ASTNode:
        """
        Evaluates the application in the given environment.
        """

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
            # records update the environment
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

    def condition(self, env) -> bool:
        return self.eval(env).condition(env)


class Integer(ASTNode):
    """
    Represents an integer value in the AST.
    Only primitive type that is supported in the language.
    """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        return self

    def condition(self, env):
        return self.value != 0


class Name(ASTNode):
    """
    Represents a  name in the AST.
    """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, env, args=None):
        """
        Evaluates the variable name in the given environment.
        If the variable is present in the environment, it returns its value.
        Otherwise, it returns the variable name itself.
        """

        if self.value in env:
            return env[self.value]
        else:
            return self

    def condition(self, env):
        if self.value in env:
            return env[self.value].condition(env)


class Pair(ASTNode):
    """
    Represents a pair of a name and an expression in the AST.
    """

    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def __str__(self):
        return f"{str(self.name)} = {str(self.expr)}"


class Record(ASTNode):
    """
        A record is a collection of pairs, that have an ordering.
        i.e.. if the record is evaluated, the pairs are evaluated in the order they are defined.
        {a= x -> plus 1 x, b = a2] --> b already knows the value of a
    """

    def __init__(self):
        self.pairs = []

    def add_pair(self, pair):
        self.pairs.append(pair)

    def eval(self, env) -> "Record":
        """
        Evaluates the record in the given environment.
        It updates the environment with the evaluated pairs.
        """
        for pair in self.pairs:
            env[pair.name.value] = pair.expr
        return self

    def eval_pairs(self, env) -> "Record":
        """
        Evaluates the record in the given environment, including the expressions in the pairs.
        It updates the environment with the evaluated pairs.
        """
        for pair in self.pairs:
            pair.expr = pair.expr.eval(env)
            env[pair.name.value] = pair.expr
        return self

    def condition(self, env):
        """
        A record is true, IFF it has at least one pair.
        """
        return self.pairs != []

    def __str__(self):
        return "{" + ", ".join([str(pair) for pair in self.pairs]) + "}"


class PredefinedFunction(ASTNode):
    """
     Represents a predefined function in the AST.
     (e.g. a function that is natively implemented in the language)
    """

    functions = {
        "add": lambda x, y: x + y,
        "plus": lambda x, y: x + y,
        "minus": lambda x, y: x - y,
        "mult": lambda x, y: x * y,
        "div": lambda x, y: x / y,
        "sub": lambda x, y: x - y,
        "cond": lambda x, y, z: y if x else z,
        "le": lambda x, y: x <= y,
        "ge": lambda x, y: x >= y,
    }

    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        if len(self.arguments) == 0:
            return str(self.name)
        return str(self.name) + " " + " ".join([str(arg) for arg in self.arguments])

    def eval(self, env: Dict[str, "ASTNode"]):
        """
        Evaluates the predefined function in the given environment.
        I.e. if the arguments are integers, it evaluates the function and returns the result.
        Otherwise, it returns the function with the arguments evaluated.
        """
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
        """
        A predefined function is true, IFF after evaluation it is not 0/{}.
        """
        return self.eval(env).condition(env)

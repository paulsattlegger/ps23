from a2.ast_nodes import *
from a2.lexer import TokenType, Lexer


class ASTBuilder:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = lexer.get_next_token()

    def eat(self, expected_type):
        if self.current_token.type == expected_type:
            self.current_token = self.lexer.get_next_token()
        else:
            raise Exception(f"Syntax Error: Expected token type {expected_type}, but got {self.current_token.type}")

    def parse(self):
        return self.parse_expr()

    def parse_expr(self):
        if self.current_token.type == TokenType.NAME and self.lexer.peek_next_token().type == TokenType.ARROW:
            param = self.parse_name()
            self.eat(TokenType.ARROW)
            expr = self.parse_expr()
            return (FunctionDeclaration(param, expr))
        return (self.parse_apply())

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

            return pairs

    def parse_name(self):
        name = self.current_token.value
        if name in ["add", "minus", "mult", "div", "cond", "plus", "sub"]:
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
        record = Record()
        while self.current_token.type != TokenType.RBRACE:
            name = self.parse_name()
            self.eat(TokenType.EQUALS)
            expr = self.parse_expr()
            record.add_pair(Pair(name, expr))
            if self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
        self.eat(TokenType.RBRACE)
        return record

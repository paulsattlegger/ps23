from enum import Enum
import re
from typing import Iterable


class TokenType(Enum):
    INTEGER = 'INTEGER'
    ARROW = 'ARROW'
    LPAREN = 'LPAREN'
    RPAREN = 'RPAREN'
    LBRACE = 'LBRACE'
    RBRACE = 'RBRACE'
    EQUALS = 'EQUALS'
    COMMA = 'COMMA'
    NAME = 'NAME'
    EOF = 'EOF'


class Token:
    def __init__(self, type: TokenType, value: str) -> None:
        self.type = type
        self.value = value

    def __str__(self) -> str:
        return f'Token({self.type}, {self.value})'

    def __repr__(self) -> str:
        return self.__str__()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Token):
            return self.type == other.type and self.value == other.value
        return False


class Lexer:

    def __init__(self, text):
        self.text = text
        self.pos = 0

    def get_next_token(self) -> Token:
        while self.pos < len(self.text):
            if self.text[self.pos].isspace():
                self.pos += 1
                continue
            elif self.text[self.pos].isdigit():
                start_pos = self.pos
                while self.pos < len(self.text) and self.text[self.pos].isdigit():
                    self.pos += 1
                return Token(TokenType.INTEGER, self.text[start_pos:self.pos])
            elif self.text[self.pos:self.pos + 2] == '->':
                self.pos += 2
                return Token(TokenType.ARROW, '->')
            elif self.text[self.pos] == '(':
                self.pos += 1
                return Token(TokenType.LPAREN, '(')
            elif self.text[self.pos] == ')':
                self.pos += 1
                return Token(TokenType.RPAREN, ')')
            elif self.text[self.pos] == '{':
                self.pos += 1
                return Token(TokenType.LBRACE, '{')
            elif self.text[self.pos] == '}':
                self.pos += 1
                return Token(TokenType.RBRACE, '}')
            elif self.text[self.pos] == '=':
                self.pos += 1
                return Token(TokenType.EQUALS, '=')
            elif self.text[self.pos] == ',':
                self.pos += 1
                return Token(TokenType.COMMA, ',')
            else:
                match = re.match(r"[A-Za-z][A-Za-z0-9]*", self.text[self.pos:])
                if match:
                    token = match.group()
                    self.pos += len(token)
                    return Token(TokenType.NAME, token)
                else:
                    raise Exception(f'Invalid character at position {self.pos}')
        return Token(TokenType.EOF, "EOF")

    def tokens(self) -> Iterable[Token]:
        while True:
            token = self.get_next_token()
            if token.type == TokenType.EOF:
                break
            yield token

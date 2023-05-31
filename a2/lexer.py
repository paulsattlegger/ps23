import re
from enum import Enum
from typing import Iterable


class TokenType(Enum):
    """
        Enum class representing the types of tokens in the lexer.
    """
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
        """
        Initializes a Token instance.

        Args:
            type: The TokenType of the token.
            value: The value of the token.
        """
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

    def __init__(self, text: str) -> None:
        """
        Initializes the Lexer with the input text.

        Args:
            text: The text to tokenize.
        """
        self.text: str = text
        self.pos: int = 0

    def get_next_token(self) -> Token:
        """
        Retrieves the next token from the input text.

        Returns:
            The next Token object.

        Raises:
            Exception: If an invalid character/token is encountered.
        """
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
        """
        Returns an iterator over the tokens in the input text.

        Yields:
            The next Token object.

        """
        while True:
            token = self.get_next_token()
            if token.type == TokenType.EOF:
                break
            yield token

    def tokens_as_list(self) -> list[Token]:
        """
        Returns a list of tokens in the input text.

        Returns:
            A list of Token objects representing the tokens in the input text.
        """
        return list(self.tokens())

    def peek_next_token(self) -> Token:
        """
        Returns the next token without consuming it.

        Returns:
            The next Token object.
        """
        current_pos = self.pos
        next_token = self.get_next_token()
        self.pos = current_pos
        return next_token

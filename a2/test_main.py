import unittest

from a2.lexer import Lexer, Token, TokenType
from main import Interpreter


class InterpreterTest(unittest.TestCase):
    def setUp(self) -> None:
        self._interpreter = Interpreter()

    def test_eval_simple(self):
        self.assertEqual(str(4), (self._interpreter.interpret_string("mult 2 2")))
        self.assertEqual(str(5), (self._interpreter.interpret_string("add 2 3")))
        self.assertEqual(str(0), (self._interpreter.interpret_string("minus 2 2")))

    def test_eval_cond(self):
        self.assertEqual("a", (self._interpreter.interpret_string("cond 1 a b")))
        self.assertEqual("b", (self._interpreter.interpret_string("cond {} a b")))
        self.assertEqual("2", (self._interpreter.interpret_string("cond 0 1 2")))

    def test_eval_complex(self):
        self.assertEqual("7", (self._interpreter.interpret_string("(x -> y -> add (mult x x) y) 2 3")))

    def test_eval_record_not_reducible(self):
        self.assertEqual(
            # our current outcome is: {a = (x -> (y -> (add (mult x x ) y ))), b = (a 2), c = (b 3)}
            # the expected outcome is: {a=x->y->add(mult x x)y, b=a 2, c=b 3} (i think the additional paranthesis are ok)
            "{a = (x -> (y -> (add (mult x x ) y ))), b = (a 2), c = (b 3)}",
            self._interpreter.interpret_string("{a=x->y->add(mult x x)y, b=a 2, c=b 3}").strip()
        )

    def test_eval_record_reducible(self):
        self.assertEqual("(y -> add 9 y)", (self._interpreter.interpret_string("(x->y->add(mult x x)y) 3")))

    def test_eval_record_environment(self):
        self.assertEqual(self._interpreter.interpret_string("{a=x->y->add(mult x x)y, b=a 2, c=b 3}minus(b 5)c"), "2")

    def test_minus_params_functions(self):
        self.assertEqual("2", self._interpreter.interpret_string(
            "minus ((x -> y -> add (mult x x) y) 2 5) ((x -> y -> add (mult x x) y) 2 3)"))

    def test_record_easy(self):
        self.assertEqual(self._interpreter.interpret_string("{a = 1, b = 2} a"), "1")

    def test_record_function_easy(self):
        self.assertEqual(self._interpreter.interpret_string("{a = x -> add x 1} a 1"), "2")

    def test_recursion_factorial(self):
        self.assertEqual(self._interpreter.interpret_string("""
        {
            factorial = n ->
                cond (minus n 1)
                    (mult n (factorial (minus n 1)))
                    1
        }
        factorial 5
        """), "120")

    # note to successfully test it, I added le: x<= y to our predefined function set
    def test_recursion_fibonacci(self):
        self.assertEqual(self._interpreter.interpret_string("""
    {
        fibonacci = n ->
            cond (le n 1)
                n
                (plus (fibonacci (sub n 1)) (fibonacci (sub n 2)))
                
    }
    fibonacci 9
        """), "34")

    def test_return_value_record_create_list(self):
        self.assertEqual("{val = 1, nxt = {val = 2, nxt = {val = 3, nxt = {val = 4, nxt = {val = 5, nxt = {val = 6, "
                         "nxt = {val = 7, nxt = {val = 8, nxt = {val = 9, nxt = {val = 10, nxt = {val = 11, "
                         "nxt = {val = 12, nxt = {val = 13, nxt = {val = 14, nxt = {}}}}}}}}}}}}}}}",
                         self._interpreter.interpret_string("""
                         {
                        list = c -> f -> x ->
                            cond (c x)
                            { val = x, nxt = list c f (f x) }
                            {}
                            ,
                         range = a -> b ->
                            list (x -> minus b x) (x -> add 1 x) a
                            } range 1 15
                         """))

    def test_eval_large_example(self):
        expr = """
        {
            list = c -> f -> x ->
                cond (c x)
                    { val = x, nxt = list c f (f x) }
                    {}
            ,
            reduce = f -> x -> lst ->
                cond lst
                    (f (reduce f x (lst nxt)) (lst val))
            x
            ,
            range = a -> b ->
                list (x -> minus b x) 
                (x -> add 1 x) a
            ,
            sum = lst ->
                reduce (x -> y -> plus x y) 0 lst
        }
        sum(range 7 15)
        """
        self.assertEqual(self._interpreter.interpret_string(expr), "84")

    def test_eval_map_example(self):
        expr = """
        {
            list = c -> f -> x ->
                cond (c x)
                    { val = x, nxt = list c f (f x) }
                    {}
            ,
            map = f -> lst ->
                cond lst
                    { val = f (lst val), nxt = map f (lst nxt) }
                    {}
            ,
            range = a -> b ->
                list (x -> minus b x) 
                (x -> add 1 x) a
            ,
            double = x -> mult x 2
        }
        map double (range 1 5)
        """
        self.assertEqual(self._interpreter.interpret_string(expr),
                         "{val = 2, nxt = {val = 4, nxt = {val = 6, nxt = {val = 8, nxt = {}}}}}")

    def test_higher_order_functions(self):
        self.assertEqual(
            self._interpreter.interpret_string("(x -> (y -> add x y)) 3 4"),
            "7"
        )
        self.assertEqual(
            self._interpreter.interpret_string("(f -> (x -> f (f x))) (x -> add x 1) 1"),
            "3"
        )
        self.assertEqual(
            self._interpreter.interpret_string("(f -> (x -> f (f (f x)))) (x -> mult x 2) 2"),
            "16"
        )


class LexerTest(unittest.TestCase):
    def test_lexer_simple(self):
        text = "(x -> y -> add (mult x x) y) 2 3"
        lexer = Lexer(text)
        tokens = list(lexer.tokens())

        expected_tokens = [
            Token(TokenType.LPAREN, '('),
            Token(TokenType.NAME, 'x'),
            Token(TokenType.ARROW, '->'),
            Token(TokenType.NAME, 'y'),
            Token(TokenType.ARROW, '->'),
            Token(TokenType.NAME, 'add'),
            Token(TokenType.LPAREN, '('),
            Token(TokenType.NAME, 'mult'),
            Token(TokenType.NAME, 'x'),
            Token(TokenType.NAME, 'x'),
            Token(TokenType.RPAREN, ')'),
            Token(TokenType.NAME, 'y'),
            Token(TokenType.RPAREN, ')'),
            Token(TokenType.INTEGER, '2'),
            Token(TokenType.INTEGER, '3'),
        ]

        self.assertEqual(tokens, expected_tokens)

    def test_lexer_complex(self):
        text = """
        {
            list = c -> f -> x ->
                cond (c x)
                    { val = x, nxt = list c f (f x) }
                    {},
            reduce = f -> x -> lst ->
                cond lst
                    (f (reduce f x (lst nxt)) (lst val))
                    x,
            range = a -> b ->
                list (x -> minus b x) (x -> plus 1 x) a,
            sum = lst ->
                reduce (x -> y -> plus x y) 0 lst
        }
        sum (range 3 6)
        """
        lexer = Lexer(text)
        tokens = list(lexer.tokens())

        expected_tokens = [
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.NAME, "list"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "c"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "f"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "cond"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "c"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.NAME, "val"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "x"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.NAME, "nxt"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "list"),
            Token(TokenType.NAME, "c"),
            Token(TokenType.NAME, "f"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "f"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.NAME, "reduce"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "f"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "cond"),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "f"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "reduce"),
            Token(TokenType.NAME, "f"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.NAME, "nxt"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.NAME, "val"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.NAME, "range"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "a"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "b"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "list"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "x"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "minus"),
            Token(TokenType.NAME, "b"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "x"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "plus"),
            Token(TokenType.INTEGER, "1"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.NAME, "a"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.NAME, "sum"),
            Token(TokenType.EQUALS, "="),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "reduce"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "x"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "y"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.NAME, "plus"),
            Token(TokenType.NAME, "x"),
            Token(TokenType.NAME, "y"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.INTEGER, "0"),
            Token(TokenType.NAME, "lst"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.NAME, "sum"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.NAME, "range"),
            Token(TokenType.INTEGER, "3"),
            Token(TokenType.INTEGER, "6"),
            Token(TokenType.RPAREN, ")")
        ]

        self.assertEqual(tokens, expected_tokens)


if __name__ == "__main__":
    unittest.main()

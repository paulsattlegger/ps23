import unittest

from main import Interpreter


class InterpreterTest(unittest.TestCase):
    def setUp(self) -> None:
        self._interpreter = Interpreter()

    def test_eval_simple(self):
        self.assertEqual(self._interpreter.eval("mult 2 2"), "4")
        self.assertEqual(self._interpreter.eval("plus 2 2"), "4")
        self.assertEqual(self._interpreter.eval("minus 2 2"), "0")

    def test_eval_cond(self):
        self.assertEqual(self._interpreter.eval("cond 0 a b"), "a")
        self.assertEqual(self._interpreter.eval("cond {} a b"), "a")

    def test_eval_complex(self):
        self.assertEqual(self._interpreter.eval("(x -> y -> add (mult x x) y) 2 3"), "7")

    def test_eval_record_not_reducible(self):
        self.assertEqual(
            self._interpreter.eval("{a=x->y->add(mult x x)y, b=a 2, c=b 3}"), "{a=x->y->add(mult x x)y, b=a 2, c=b 3}"
        )

    def test_eval_record_reducible(self):
        self.assertEqual(self._interpreter.eval("(x->y->add(mult~x~x)y) 3"), "y->add 9 y")

    def test_eval_record_environment(self):
        self.assertEqual(self._interpreter.eval("{a=x->y->add(mult x x)y, b=a 2, c=b 3}minus(b 5)c"), "y->add 9 y", 2)

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
                list (x -> minus b x) (x -> plus 1 x) a
            ,
            sum = lst ->
                reduce (x -> y -> plus x y) 0 lst
        }
        sum (range 3 6)
        """
        self.assertEqual(self._interpreter.eval(expr), "12")


if __name__ == "__main__":
    unittest.main()

import unittest
from unittest.mock import patch

from main import Calculator


class CalculatorTestCase(unittest.TestCase):
    # NOTE: other than in the main.py, the calculator is initialized without calling b-register recursively
    def setUp(self) -> None:
        self._calculator: Calculator = Calculator()
        self._calculator.register["a"] = '(Welcome)"b@'
        self._calculator.register["b"] = "'@"
        for token in self._calculator.register["a"][::-1]:
            self._calculator.cmd.push(token)

    # NOTE: to check the value of the mean calculation, the output-write is removed from register c (" at the end)
    @patch("builtins.input", side_effect=["c@", "1 2 3", " "])
    def test_mean(self, _) -> None:
        self._calculator.register["c"] = "'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/"
        self._calculator.run()
        self.assertEqual(2, self._calculator.data.pop())

    @patch("builtins.input", side_effect=["c@", "0.314 1.546 2.718 3.141 4.669 5.432 6.283 7.777 8.965 9.876", " "])
    def test_mean_floats(self, _) -> None:
        self._calculator.register["c"] = "'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/"
        self._calculator.run()
        self.assertAlmostEquals(5.0721, self._calculator.data.pop())

    @patch("builtins.input", side_effect=["e@", "1.0 2.0 3.0 4.0 5.0 6.0 7.0", " "])
    def test_variance_simple(self, _) -> None:
        self._calculator.register[
            "e"
        ] = "'2!@@#1+!@#$#2/#2/0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/#2-#2-0()(8!8$8!-2!*5!5$+7!7$7!7$7!7$1-2!_~6+$5!5$()7!7$2!@)2!@1$1$2$3!3$/2$"
        self._calculator.run()
        self.assertAlmostEquals(4.666666666666667, self._calculator.data.pop())

    @patch("builtins.input", side_effect=["e@", "2.1512 3.5 4.124 1234.124 421.2 32.1 45.66 213.421", " "])
    def test_variance_floats(self, _) -> None:
        self._calculator.register[
            "e"
        ] = "'2!@@#1+!@#$#2/#2/0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/#2-#2-0()(8!8$8!-2!*5!5$+7!7$7!7$7!7$1-2!_~6+$5!5$()7!7$2!@)2!@1$1$2$3!3$/2$"
        self._calculator.run()
        self.assertAlmostEquals(181541.44480577632, self._calculator.data.pop())


if __name__ == "__main__":
    unittest.main()

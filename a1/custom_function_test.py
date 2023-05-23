import unittest
from unittest.mock import patch

from a1.main import Calculator


class CalculatorTestCase(unittest.TestCase):

    # NOTE other than in the main.py, the calculator is initialized without calling b-register recursively
    def setUp(self) -> None:
        self._calculator: Calculator = Calculator()
        self._calculator.register["a"] = '(Welcome)"b@'
        self._calculator.register["b"] = '\'@'
        for token in self._calculator.register["a"][::-1]:
            self._calculator.cmd.push(token)

    # NOTE: to check the value of the mean calculation, the output-write is removed from register c (" at the end)
    @patch('builtins.input', side_effect=['c@', '1 2 3', ' '])
    def test_mean(self, mock_in) -> None:
        self._calculator.register["c"] = '\'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/'
        self._calculator.run()
        self.assertEqual(2, self._calculator.data.pop())

    @patch('builtins.input', side_effect=['c@', '0.314 1.546 2.718 3.141 4.669 5.432 6.283 7.777 8.965 9.876', ' '])
    def test_mean_floats(self, mock_in) -> None:
        self._calculator.register["c"] = '\'@##1-0()(7!7$5!5$+6!6$6!6$1-2!_~5+$4!4$()6!6$2!@)2!@1$1$2$3!3$/'
        self._calculator.run()
        self.assertAlmostEquals(5.0721, self._calculator.data.pop())

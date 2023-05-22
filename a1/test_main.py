import unittest

from main import Calculator


class CalculatorTestCase(unittest.TestCase):
    def setUp(self) -> None:
        self._calculator: Calculator = Calculator()

    def test_add(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(5)
        self._calculator.cmd.push("+")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 10)

    def test_lt(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(4)
        self._calculator.cmd.push("<")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), True)


if __name__ == "__main__":
    unittest.main()

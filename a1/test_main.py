import unittest

from main import Calculator


class CalculatorTestCase(unittest.TestCase):
    def setUp(self) -> None:
        self._calculator: Calculator = Calculator()

    # Test arithmetic operations
    def test_add(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(5)
        self._calculator.cmd.push("+")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 10)

    def test_sub(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(6)
        self._calculator.cmd.push("-")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), -1)

    def test_mul(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(6)
        self._calculator.cmd.push("*")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 30)

    def test_div_int(self) -> None:
        self._calculator.data.push(6)
        self._calculator.data.push(5)
        self._calculator.cmd.push("/")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_div_float(self) -> None:
        self._calculator.data.push(6.0)
        self._calculator.data.push(5.0)
        self._calculator.cmd.push("/")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1.2)

    def test_div_0(self) -> None:
        self._calculator.data.push(6)
        self._calculator.data.push(0)
        self._calculator.cmd.push("/")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), "()")

    def test_mod(self) -> None:
        self._calculator.data.push(6)
        self._calculator.data.push(5)
        self._calculator.cmd.push("%")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_mod_float(self) -> None:
        self._calculator.data.push(6.0)
        self._calculator.data.push(5.0)
        self._calculator.cmd.push("%")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), "()")

    # test logical operators
    def test_and_true(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(1)
        self._calculator.cmd.push("&")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_and_false(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(0)
        self._calculator.cmd.push("&")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 0)

    def test_or_true(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(0)
        self._calculator.cmd.push("|")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_or_false(self) -> None:
        self._calculator.data.push(0)
        self._calculator.data.push(0)
        self._calculator.cmd.push("|")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 0)

    # null check
    def test_null(self) -> None:
        self._calculator.data.push(0)
        self._calculator.cmd.push("_")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

        self._calculator.data.push("()")
        self._calculator.cmd.push("_")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_not_null(self) -> None:
        self._calculator.data.push(1)
        self._calculator.cmd.push("_")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 0)

        self._calculator.data.push(0.1)
        self._calculator.cmd.push("_")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 0)

        self._calculator.data.push("ewe")
        self._calculator.cmd.push("_")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 0)

    # negation tests
    def test_negate(self) -> None:
        self._calculator.data.push(5)
        self._calculator.cmd.push("~")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), -5)

        self._calculator.data.push(3.1)
        self._calculator.cmd.push("~")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), -3.1)

    def test_negate_string(self) -> None:
        self._calculator.data.push("ewe")
        self._calculator.cmd.push("~")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), "()")

    # test copy
    def test_copy(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(3)
        self._calculator.data.push(2)
        self._calculator.cmd.push("!")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 3)

    def test_int_conversion(self) -> None:
        self._calculator.data.push(5.0)
        self._calculator.cmd.push("?")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 5)

    def test_delete(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(2)
        self._calculator.data.push(3)
        self._calculator.data.push(4)
        self._calculator.data.push(2)

        self._calculator.cmd.push("$")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 4)
        self.assertEqual(self._calculator.data.pop(), 2)
        self.assertEqual(self._calculator.data.pop(), 1)

    def test_stack_size(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(2)
        self._calculator.data.push(3)
        self._calculator.data.push(4)

        self._calculator.cmd.push("#")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 4)

    def test_apply_immediately(self) -> None:
        self._calculator.data.push(2)
        self._calculator.data.push(3)
        self._calculator.data.push("-")

        self._calculator.cmd.push("@")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), -1)

    def test_apply_immediately_no_string(self) -> None:
        self._calculator.data.push("ewe")
        self._calculator.data.push(3)

        self._calculator.cmd.push("@")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), 3)
        self.assertEqual(self._calculator.data.pop(), "ewe")

    def test_apply_later(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push(1)
        self._calculator.data.push(5)
        self._calculator.data.push("+")
        self._calculator.cmd.push("-")
        self._calculator.cmd.push("\\")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), -3)

    def test_assignment_example1(self) -> None:
        self._calculator.data.push(1)
        self._calculator.data.push("(8)(9~)(4!4$_1+$@)@")
        self._calculator.cmd.push("@")
        self._calculator.run()

        self.assertEqual(self._calculator.data.pop(), 8)

    def test_assignment_example2_fac5(self) -> None:
        self._calculator.data.push(15)
        self._calculator.data.push("(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$")
        self._calculator.cmd.push("@")
        self._calculator.run()

        self.assertEqual(self._calculator.data.pop(), 1307674368000)

    # Test comparison operators
    def test_lt(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(4)
        self._calculator.cmd.push("<")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), True)

    def test_gt(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(4)
        self._calculator.cmd.push(">")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), False)

    def test_eq(self) -> None:
        self._calculator.data.push(5)
        self._calculator.data.push(5)
        self._calculator.cmd.push("=")
        self._calculator.run()
        self.assertEqual(self._calculator.data.pop(), True)


if __name__ == "__main__":
    unittest.main()

    # MEDIAN attempts on unsorted list
    # calculator.register["g"] = "(7!7$0 5!5$10!10$2!2!2/>)"
    # calculator.register["h"] = "(##1-#2-0 0()(8!8$0 5!5$11!11$7!4!4!2/1+=&7!7$4!4!2/>&|)()(Code)8!10+!10!12+!2!2!=2!4!5!<|3$9!9$+10!10$1-2!_~8+$8!8$@)"

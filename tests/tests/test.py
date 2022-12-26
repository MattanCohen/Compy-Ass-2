import sys
import os
from ast import literal_eval
import unittest

sys.path.insert(0, os.getcwd())
from tests.ocaml_framework.framework import compile_module
from tests.ocaml_framework.compiler_data_types import ParsingResult, TagParserModule

module: TagParserModule = compile_module()
X_no_match = module.PC.X_no_match


class ParserTestCase(unittest.TestCase):
    @staticmethod
    def is_parsing_result(x):
        return hasattr(x, 'index_from') and isinstance(x.index_from, int) and hasattr(x, 'index_to') and isinstance(
            x.index_to, int) and hasattr(x, 'found')

    def assertResultEquals(self, res: ParsingResult, index_from: int, index_to: int, found):
        if not ParserTestCase.is_parsing_result(res):
            self.fail(f"variable: {res} is not a parsing result!")
        self.assertEqual(res.index_from, index_from)
        self.assertEqual(res.index_to, index_to)
        if hasattr(res.found, "f0"):
            self.assertEqual(res.found.f0, found)
        else:
            self.assertEqual(res.found, found)


class TestGCD(unittest.TestCase):
    def test_gcd(self):
        self.assertEqual(module.gcd(8, 12), 4)


class TestNTDigit(unittest.TestCase):
    def test_nt_digit_parses_all_digits(self):
        s = "1234567890"
        for i in range(10):
            res = module.nt_digit(s, i)
            self.assertEqual(res.index_from, i)
            self.assertEqual(res.index_to, i + 1)
            self.assertEqual(res.found, int(s[i]))

    def test_nt_digit_raises_when_needs(self):
        s = "hi12354"
        self.assertRaises(X_no_match, module.nt_digit, s, 0)
        self.assertRaises(X_no_match, module.nt_digit, s, 1)

        for i in range(2, len(s)):
            res = module.nt_digit(s, i)
            self.assertEqual(res.index_from, i)
            self.assertEqual(res.index_to, i + 1)
            self.assertEqual(res.found, int(s[i]))


class TestNTHexDigit(unittest.TestCase):
    def test_nt_hex_digit_parses_all_digits(self):
        s = "1234567890abcdefABCDEF"
        for i in range(len(s)):
            res = module.nt_hex_digit(s, i)
            self.assertEqual(res.index_from, i)
            self.assertEqual(res.index_to, i + 1)
            self.assertEqual(res.found, literal_eval(f"0x{s[i]}"))

    def test_nt_hex_digit_raises_when_needs(self):
        s = "hiHI12354aAbBcC"
        for i in range(4):
            self.assertRaises(X_no_match, module.nt_hex_digit, s, i)

        for i in range(4, len(s)):
            res = module.nt_hex_digit(s, i)
            self.assertEqual(res.index_from, i)
            self.assertEqual(res.index_to, i + 1)
            self.assertEqual(res.found, literal_eval(f"0x{s[i]}"))


class TestNTNat(unittest.TestCase):
    def test_nt_nat_digit_parses_all_numbers(self):
        s = "1234567890"
        for i in range(len(s)):
            res = module.nt_nat(s, i)
            self.assertEqual(res.index_from, i)
            self.assertEqual(res.index_to, len(s))
            self.assertEqual(res.found, int(s[i:]))

    def test_nt_nat_digit_raises_when_needs(self):
        s = "AAA1234567890BBB"
        last_digit_pos = max((i for i in range(len(s)) if module.is_decimal_digit(s[i])))
        for i in range(len(s)):
            if module.is_decimal_digit(s[i]):
                res = module.nt_nat(s, i)
                self.assertEqual(res.index_from, i)
                self.assertEqual(res.index_to, last_digit_pos + 1)
                self.assertEqual(res.found, int(s[i:last_digit_pos + 1]))
            else:
                self.assertRaises(X_no_match, module.nt_nat, s, i)


class TestNTOptionalSign(unittest.TestCase):
    def test_plus(self):
        s = "+12345"
        res = module.nt_optional_sign(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, 1)
        self.assertEqual(res.found, True)

    def test_minus(self):
        s = "-12345"
        res = module.nt_optional_sign(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, 1)
        self.assertEqual(res.found, False)

    def test_no_sign(self):
        s = "12345"
        res = module.nt_optional_sign(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, 0)
        self.assertEqual(res.found, True)


class TestNTFloat(ParserTestCase):
    def test_float_a_1(self):
        s = "-0124."
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_a_2(self):
        s = "-0124.123456789"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_a_3(self):
        s = "-0124.e4"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_a_4(self):
        s = "-0124.123456789e-54"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_b_1(self):
        s = "-.12345678"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_b_2(self):
        s = ".12345678e-54"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_c_1(self):
        s = "2e-4"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))

    def test_float_c_2(self):
        s = "-7e4"
        self.assertResultEquals(module.nt_float(s, 0), 0, len(s), float(s))


class TestNTCharNamed(ParserTestCase):
    def test_nt_newline(self):
        self.assertResultEquals(module.nt_char_named("\n", 0), 0, 1, "\n")

    def test_nt_nul(self):
        self.assertResultEquals(module.nt_char_named("\0", 0), 0, 1, "\0")

    def test_nt_page(self):
        s = chr(12)
        self.assertResultEquals(module.nt_char_named(s, 0), 0, 1, s)

    def test_nt_return(self):
        s = chr(13)
        self.assertResultEquals(module.nt_char_named(s, 0), 0, 1, s)

    def test_nt_space(self):
        s = " "
        self.assertResultEquals(module.nt_char_named(s, 0), 0, 1, s)

    def test_nt_tab(self):
        s = chr(9)
        self.assertResultEquals(module.nt_char_named(s, 0), 0, 1, s)


class TestNTSymbol(ParserTestCase):
    def test_nt_symbol(self):
        s = "$asd"
        self.assertResultEquals(module.nt_symbol(s, 0), 0, len(s), s)


class TestNTComment(ParserTestCase):
    def test_nt_paired_comment_1(self):
        s = "{hellooo hiiiiii}"
        self.assertResultEquals(module.nt_paired_comment(s, 0), 0, len(s), None)

    def test_nt_paired_comment_2(self):
        s = "{hellooo hiiiiii}{"
        self.assertResultEquals(module.nt_paired_comment(s, 0), 0, len(s) - 1, None)

    def test_nt_paired_comment_4(self):
        s = r"{hellooo hiiiiii{}"
        self.assertRaises(X_no_match, module.nt_paired_comment, s, 0)

    def test_nt_paired_comment_5(self):
        s = r"{hellooo hiiiiii#\{}"
        self.assertResultEquals(module.nt_paired_comment(s, 0), 0, len(s), None)

    def test_nt_paired_comment_6(self):
        s = r"{hellooo hiiiiii#\}}"
        self.assertResultEquals(module.nt_paired_comment(s, 0), 0, len(s), None)

    def test_nt_sexp_comment(self):
        s = "#;(+ 1 2)"
        self.assertResultEquals(module.nt_sexpr_comment(s, 0), 0, len(s), None)


class TestNTString(ParserTestCase):
    def test_nt_string_dynamic_part(self):
        s = '"~{(+ 1 2)}"'
        res = module.nt_sexpr(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, len(s))


class TestNTList(ParserTestCase):
    def test_nt_sexpr(self):
        s = "(1 2 3)"
        res = module.nt_sexpr(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, len(s))

class TestAnd(ParserTestCase):
    def test_and_1(self):
        s = "(and 1 2 #f)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], False)

    def test_and_2(self):
        s = "(and 1 2 3)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], True)

    def test_and_3(self):
        s = "(and 1 2 #t)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], True)

    def test_and_4(self):
        s = "(and 1)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], True)

    def test_and_5(self):
        s = "(and)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], True)




if __name__ == "__main__":
    unittest.main()

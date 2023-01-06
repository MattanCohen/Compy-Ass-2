import sys
import os
from ast import literal_eval
import unittest



sys.path.insert(0, os.getcwd())
from tests.ocaml_framework.scheme_types import *
from dataclasses import is_dataclass
from tests.ocaml_framework.framework import compile_module
from tests.ocaml_framework.compiler_data_types import ParsingResult, TagParserModule

module = compile_module()
X_no_match = module.PC.X_no_match




class CompilerTestCase(unittest.TestCase):
    @staticmethod
    def is_parsing_result(x):
        return hasattr(x, 'index_from') and isinstance(x.index_from, int) and hasattr(x, 'index_to') and isinstance(
            x.index_to, int) and hasattr(x, 'found')

    def assertResultEquals(self, res: ParsingResult, index_from: int, index_to: int, found):
        if not CompilerTestCase.is_parsing_result(res):
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


class TestNTFloat(CompilerTestCase):
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


class TestNTCharNamed(CompilerTestCase):
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


class TestNTSymbol(CompilerTestCase):
    def test_nt_symbol(self):
        s = "$asd"
        self.assertResultEquals(module.nt_symbol(s, 0), 0, len(s), s)


class TestNTComment(CompilerTestCase):
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


class TestNTString(CompilerTestCase):
    def test_nt_string_dynamic_part(self):
        s = '"~{(+ 1 2)}"'
        res = module.nt_sexpr(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, len(s))


class TestNTList(CompilerTestCase):
    def test_nt_sexpr(self):
        s = "(1 2 3)"
        res = module.nt_sexpr(s, 0)
        self.assertEqual(res.index_from, 0)
        self.assertEqual(res.index_to, len(s))


class TestAnd(CompilerTestCase):
    def test_and_1(self):
        s = "(and 1 2 #f)"
        res = module.nt_sexpr(s, 0)
        fixed_res: ScmIf = module.tag_parse(res.found)
        if_exp = validate_ocaml_tagged(fixed_res, ScmIf)
        exp1 = if_exp.f0

        exp1_numeric_value = scm_number_to_number(validate_scm_number(validate_scm_const(exp1).f0))
        self.assertEqual(1, exp1_numeric_value)

        exp2 = validate_ocaml_tagged(if_exp.f1, ScmIf)
        self.assertEqual(2, scm_number_to_number(validate_scm_number(validate_scm_const(exp2.f0).f0)))
        self.assertEqual(False, validate_scm_bool(validate_scm_const(exp2.f1).f0).f0)


        exp3 = validate_scm_const(if_exp.f2)
        self.assertEqual(False, validate_scm_bool(exp3.f0).f0)
        #self.assertEqual(2, scm_number_to_number(validate_scm_number(validate_ocaml_tagged(validate_ocaml_tagged(fixed_res, ScmIf).f0, ScmConst).f0)))
        #self.assertEqual(False, scheme_bool_to_bool(fixed_res[2][0]))

    def test_and_2(self):
        s = "(and 1 2 3)"
        res = module.nt_sexpr(s, 0)
        if_exp: ScmIf = validate_ocaml_tagged(module.tag_parse(res.found), ScmIf)

        exp1 = if_exp.f0
        exp1_numeric_value = scm_number_to_number(validate_scm_number(validate_scm_const(exp1).f0))
        self.assertEqual(1, exp1_numeric_value)

        exp2 = validate_ocaml_tagged(if_exp.f1, ScmIf)
        self.assertEqual(2, scm_number_to_number(validate_scm_number(validate_scm_const(exp2.f0).f0)))
        self.assertEqual(3, scm_number_to_number(validate_scm_number(validate_scm_const(exp2.f1).f0)))


        exp3 = validate_scm_const(if_exp.f2)
        self.assertEqual(False, validate_scm_bool(exp3.f0).f0)

    def test_and_3(self):
        s = "(and 1 2 #t)"
        res = module.nt_sexpr(s, 0)
        if_exp: ScmIf = validate_ocaml_tagged(module.tag_parse(res.found), ScmIf)

        exp1 = if_exp.f0
        exp1_numeric_value = scm_number_to_number(validate_scm_number(validate_scm_const(exp1).f0))
        self.assertEqual(1, exp1_numeric_value)

        exp2 = validate_ocaml_tagged(if_exp.f1, ScmIf)
        self.assertEqual(2, scm_number_to_number(validate_scm_number(validate_scm_const(exp2.f0).f0)))
        self.assertEqual(True, validate_scm_bool(validate_scm_const(exp2.f1).f0).f0)


        exp3 = validate_scm_const(if_exp.f2)
        self.assertEqual(False, validate_scm_bool(exp3.f0).f0)

    # def test_and_4(self):
    #     s = "(and 1)"
    #     res = module.nt_sexpr(s, 0)
    #     if_exp: ScmIf = validate_ocaml_tagged(module.tag_parse(res.found), ScmIf)

    #     exp1 = if_exp.f0
    #     exp1_numeric_value = scm_number_to_number(validate_scm_number(validate_scm_const(exp1).f0))
    #     self.assertEqual(1, exp1_numeric_value)

    #     exp2 = validate_ocaml_tagged(if_exp.f1, ScmIf)
    #     self.assertEqual(2, scm_number_to_number(validate_scm_number(validate_scm_const(exp2.f0).f0)))
    #     self.assertEqual(True, validate_scm_bool(validate_scm_const(exp2.f1).f0).f0)

    #     exp3 = validate_scm_const(if_exp.f2)
    #     self.assertEqual(False, validate_scm_bool(exp3.f0).f0)

    def test_and_5(self):
        s = "(and)"
        res = module.nt_sexpr(s, 0)
        fixed_res = module.tag_parse(res.found)
        self.assertEqual(fixed_res[0][0], True)


class TestCond(CompilerTestCase):
    # def test_empty_cond(self):
    #     s = "(cond)"
    #     res = module.nt_sexpr(s, 0)
    #     parsed = module.tag_parse(res.found)
    #     print(parsed)

    def test_cond_1(self):
        s = "(cond (#t 1))"
        res = module.nt_sexpr(s, 0)
        parsed = module.tag_parse(res.found)
        supposed = 'ScmIf(ScmConst(ScmBoolean(True)),ScmConst(ScmNumber(ScmRational((1,1)))),ScmConst(ScmVoid))'
        self.assertEqual(str(parsed), supposed)


    def test_cond_2(self):
        s = "(cond)"
        res = module.nt_sexpr(s, 0)
        parsed = module.tag_parse(res.found)
        supposed = 'ScmConst(ScmVoid)'
        self.assertEqual(str(parsed), supposed)

    def test_cond_3(self):
        s = "(cond (#t 1))"
        res = module.nt_sexpr(s, 0)
        parsed = module.tag_parse(res.found)
        supposed = 'ScmIf(ScmConst(ScmBoolean(True)),ScmConst(ScmNumber(ScmRational((1,1)))),ScmConst(ScmVoid))'
        self.assertEqual(str(parsed), supposed)


class TestLet(CompilerTestCase):
    
    def test_let_complicated_1(self):
        lambdaExp = "((lambda (x) (lambda (y) y)) 5)"
        letExp = "(let ((x 5)) (lambda (y) y))"
        lambdaRes = module.nt_sexpr(lambdaExp, 0)
        letRes = module.nt_sexpr(letExp, 0)


        parsedLambda = module.tag_parse(lambdaRes.found)
        parsedLet = module.tag_parse(letRes.found)

        debugLet = False; 
        if (debugLet):
            print(f"test let complicated 1 {letExp} = {lambdaExp}")
            print(f"lambda res: {lambdaRes.found}")
            print(f"let res:    {letRes.found}")
            print(f"lambda parsed: {parsedLambda}")
            print(f"let parsed:    {parsedLet}")
            print("")

        self.assertEqual(str(parsedLambda), str(parsedLet))
    
    def test_let_complicated_2(self):
        lambdaExp = "((lambda (x) ((lambda (y) y) 5) ) 2)"
        letExp = "(let ((x 2)) (let ((y 5)) y))"
        lambdaRes = module.nt_sexpr(lambdaExp, 0)
        letRes = module.nt_sexpr(letExp, 0)


        parsedLambda = module.tag_parse(lambdaRes.found)
        parsedLet = module.tag_parse(letRes.found)

        debugLet = False; 
        if (debugLet):
            print(f"test let complicated 2 {letExp} = {lambdaExp}")
            print(f"lambda res: {lambdaRes.found}")
            print(f"let res:    {letRes.found}")
            print(f"lambda parsed: {parsedLambda}")
            print(f"let parsed:    {parsedLet}")
            print("")

        self.assertEqual(str(parsedLambda), str(parsedLet))


    def test_letstar_1(self):
        lambdaExp = "((lambda (x) x) 2)"
        letExp = "(let* ((x 2)) x)"
        lambdaRes = module.nt_sexpr(lambdaExp, 0)
        letRes = module.nt_sexpr(letExp, 0)

        

        parsedLambda = module.tag_parse(lambdaRes.found)
        parsedLet = module.tag_parse(letRes.found)
        self.assertEqual(str(parsedLambda), str(parsedLet))

        
        debugLet = False; 
        if (debugLet):
            print(f"test letstar 1: (let* ((x 2)) x) = ((lambda (x) x) 2)")
            print(f"lambda res: {lambdaRes.found}")
            print(f"let res:    {letRes.found}")
            print(f"lambda parsed: {parsedLambda}")
            print(f"let parsed:    {parsedLet}")
            print("")
        
    def test_letstar_2(self):
        lambdaExp = "((lambda () 2))"
        letExp = "(let* () 2)"
        lambdaRes = module.nt_sexpr(lambdaExp, 0)
        letRes = module.nt_sexpr(letExp, 0)


        parsedLambda = module.tag_parse(lambdaRes.found)
        parsedLet = module.tag_parse(letRes.found)

        debugLet = False; 
        if (debugLet):
            print(f"test letstar 2 {letExp} = {lambdaExp}")
            print(f"lambda res: {lambdaRes.found}")
            print(f"let res:    {letRes.found}")
            print(f"lambda parsed: {parsedLambda}")
            print(f"let parsed:    {parsedLet}")
            print("")

        self.assertEqual(str(parsedLambda), str(parsedLet))
        

    def test_letstar_3(self):
        lambdaExp = "(let ((x 2)) (let ((y 4)) x))"
        letExp = "(let* ((x 2) (y 4)) x)"
        lambdaRes = module.nt_sexpr(lambdaExp, 0)
        letRes = module.nt_sexpr(letExp, 0)


        parsedLambda = module.tag_parse(lambdaRes.found)
        parsedLet = module.tag_parse(letRes.found)

        
        debugLet = False; 
        if (debugLet):
            print(f"test letstar 3 {letExp} = {lambdaExp}")
            print(f"lambda res: {lambdaRes.found}")
            print(f"let res:    {letRes.found}")
            print(f"lambda parsed: {parsedLambda}")
            print(f"let parsed:    {parsedLet}")
            print("")

        self.assertEqual(str(parsedLambda), str(parsedLet))

        

    def test_letrec(self):
        letExp = "(letrec ((x 2) (y x)) y)"
        letRes = module.nt_sexpr(letExp, 0)


        parsedLet = module.tag_parse(letRes.found)

        
        debugLet = False; 
        if (debugLet):
            print(f"let res:    {letRes.found}")
            print(f"let parsed:    {parsedLet}")
            print("")


        



if __name__ == "__main__":
    unittest.main()

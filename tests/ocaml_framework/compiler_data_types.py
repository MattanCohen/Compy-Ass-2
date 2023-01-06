from dataclasses import dataclass
from typing import TypeVar, Generic, Callable, TextIO, Type

T = TypeVar("T")

Number = int | float
SExp = None | bool | str | Number | list['SExp'] | tuple['SExp', 'SExp']


@dataclass
class ParsingResult(Generic[T]):
    index_from: int
    index_to: int
    found: T


Parser = Callable[[str, int], ParsingResult[T]]

ATTRIBUTES_OUTSIDE_OF_READER = ["PC", "gcd"]


@dataclass
class PC:
    X_no_match: Type[BaseException]


@dataclass
class ReaderModule:
    PC: PC
    gcd: Callable[[int, int], int]
    print_sexpr: Callable[[SExp, TextIO], None]
    print_sexprs: Callable[[list[SExp], TextIO], None]
    sprint_sexpr: Callable[[any, SExp], str]
    sprint_sexprs: Callable[[any, list[SExp]], str]
    scheme_sexpr_list_of_sexpr_list: Callable[[list[SExp]], SExp]
    nt_digit: Parser[int]
    nt_hex_digit: Parser[int]
    nt_nat: Parser[int]
    is_decimal_digit: Callable[[str], bool]
    nt_optional_sign: Parser[bool]
    nt_float: Parser[float]
    nt_char_named: Parser[str]
    nt_symbol: Parser[str]
    nt_paired_comment: Parser[None]
    nt_sexpr_comment: Parser[None]
    nt_left_curly_bracket: Parser[str]
    nt_sexpr: Parser[SExp]
    nt_sexpr_comment: Parser[None]

@dataclass
class TagParserModule:
    tag_parse: Callable[[SExp], SExp]
    macro_expand_cond_ribs: Callable[[SExp], SExp]

@dataclass
class SemanticAnalysisModule:
    pass

@dataclass
class CodeGenerationModule:
    pass

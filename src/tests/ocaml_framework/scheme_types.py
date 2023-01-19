import __future__
from dataclasses import dataclass
from types import UnionType
from typing import TypeVar, Generic, Literal

from tests.ocaml_framework.compiler_data_types import Number

OcamlPrimitive = None | bool | str | Number | list['OcamlPrimitive'] | tuple['OcamlPrimitive', ...]
T = TypeVar("T")


def validate_ocaml_tagged(x, tagged_type: type[T]) -> T:
    type_name = tagged_type.__name__
    assert hasattr(x, '_constructor_name') and x._constructor_name == type_name, f'Not an {type_name}: {x}'
    return x


def validate_ocaml_tagged_union(x, *tagged_types: type[T]) -> T:
    assert hasattr(x, '_constructor_name'), f'Not an Ocaml Tagged type: {x}'
    type_name: str = x._constructor_name
    for t in tagged_types:
        if type_name == t.__name__:
            return x

    raise AssertionError(f'Not a union of {[t.__name__ for t in tagged_types]}')


@dataclass
class ScmRational:
    _constructor_name: Literal["ScmRational"]
    f0: tuple[int, int]


def validate_scm_rational(x) -> ScmRational:
    return validate_ocaml_tagged(x, ScmRational)


@dataclass
class ScmReal:
    _constructor_name: Literal["ScmReal"]
    f0: float


def validate_scm_real(x) -> ScmReal:
    return validate_ocaml_tagged(x, ScmReal)


scm_number = ScmRational | ScmReal


def validate_scm_number_int(x) -> scm_number:
    return validate_ocaml_tagged_union(x, ScmRational, ScmReal)


@dataclass
class ScmVoid:
    _constructor_name: Literal["ScmVoid"]


def validate_scm_void(x) -> ScmVoid:
    return validate_ocaml_tagged(x, ScmVoid)


@dataclass
class ScmNil:
    _constructor_name: Literal["ScmNil"]


def validate_scm_nil(x) -> ScmNil:
    return validate_ocaml_tagged(x, ScmNil)


@dataclass
class ScmBoolean:
    _constructor_name: Literal["ScmBoolean"]
    f0: bool


def validate_scm_bool(x) -> ScmBoolean:
    return validate_ocaml_tagged(x, ScmBoolean)


@dataclass
class ScmChar:
    _constructor_name: Literal["ScmChar"]
    f0: str


def validate_scm_char(x) -> ScmChar:
    return validate_ocaml_tagged(x, ScmChar)


@dataclass
class ScmString:
    _constructor_name: Literal["ScmString"]
    f0: str


def validate_scm_string(x) -> ScmString:
    return validate_ocaml_tagged(x, ScmString)


@dataclass
class ScmSymbol:
    _constructor_name: Literal["ScmSymbol"]
    f0: str


def validate_scm_symbol(x) -> ScmSymbol:
    return validate_ocaml_tagged(x, ScmSymbol)


@dataclass
class ScmNumber:
    _constructor_name: Literal["ScmNumber"]
    f0: scm_number


def scm_number_to_number(x: ScmNumber) -> Number:
    try:
        rational = validate_scm_rational(x.f0)
        return float(rational.f0[0]) / float(rational.f0[1])
    except AssertionError:
        real = validate_scm_real(x)
        return real.f0


def validate_scm_number(x) -> ScmNumber:
    return validate_ocaml_tagged(x, ScmNumber)


@dataclass
class ScmVector:
    _constructor_name: Literal["ScmVector"]
    f0: list["ScmSExp"]


@dataclass
class ScmPair:
    _constructor_name: Literal["ScmPair"]
    f0: tuple["ScmSExp", "ScmSExp"]


@dataclass
class ScmSExp:
    _constructor_name: Literal["ScmSExp"]
    f0: ScmVoid | ScmNil | ScmBoolean | ScmChar | ScmString | ScmSymbol | ScmNumber | ScmVector | ScmPair


@dataclass
class ScmVar:
    _constructor_name: Literal["Var"]
    f0: str


@dataclass
class SimpleLambda:
    _constructor_name: Literal["Simple"]


@dataclass
class OptLambda:
    _constructor_name: Literal["Opt"]
    f0: str


LambdaKind = SimpleLambda | OptLambda


@dataclass
class ScmConst:
    _constructor_name: Literal["ScmConst"]
    f0: ScmSExp


def validate_scm_const(x) -> ScmConst:
    return validate_ocaml_tagged(x, ScmConst)


@dataclass
class ScmVarGet:
    _constructor_name: Literal["ScmVarGet"]
    f0: ScmVar


@dataclass
class ScmIf:
    _constructor_name: Literal["ScmIf"]
    f0: "Expr"
    f1: "Expr"
    f2: "Expr"


@dataclass
class ScmSeq:
    _constructor_name: Literal["ScmSeq"]
    f0: list["Expr"]


@dataclass
class ScmOr:
    _constructor_name: Literal["ScmOr"]
    f0: list["Expr"]


@dataclass
class ScmVarSet:
    _constructor_name: Literal["ScmVarSet"]
    f0: ScmVar
    f1: "Expr"


@dataclass
class ScmVarDef:
    _constructor_name: Literal["ScmVarDef"]
    f0: ScmVar
    f1: "Expr"


@dataclass
class ScmLambda:
    _constructor_name: Literal["ScmLambda"]
    f0: list[str]
    f1: LambdaKind
    f2: "Expr"


@dataclass
class ScmApplic:
    _constructor_name: Literal["ScmApplic"]
    f0: "Expr"
    f1: list["Expr"]


Expr = ScmConst | ScmVarGet | ScmIf | ScmSeq | ScmOr | ScmVarSet | ScmVarDef | ScmLambda | ScmApplic


def is_ocaml_primitive(x):
    return not hasattr(x, '_constructor_name')

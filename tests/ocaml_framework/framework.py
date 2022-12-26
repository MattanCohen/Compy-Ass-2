import functools
import inspect
import ocaml
import pathlib
from tests.ocaml_framework.compiler_data_types import *

OCAML_FILES_IN_COMPILING_ORDER = ["src/pc.ml", "src/compiler.ml"]

T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")


def compose(f2: Callable[[T2], T3], f1: Callable[[T1], T2] | Callable[[], T2]) -> Callable[[T1], T3]:
    if inspect.getfullargspec(f1).args:
        return lambda x: f2(f1(x))
    else:
        return lambda: f2(f1())


def pipe(*fs: Callable):
    return functools.reduce(compose, fs)


def read_all_files() -> str:
    return "\n".join((pathlib.Path(filename).read_text() for filename in OCAML_FILES_IN_COMPILING_ORDER))


def fix_line(line: str):
    if line.find("module Reader") == 0:
        return "module Reader = struct"
    if line.find("module Tag_Parser") == 0:
        return "module Tag_Parser = struct"

    return line


def filter_line(line: str):
    return line.find("#use") != 0


def fix_for_tests(content: str) -> str:
    return "\n".join((fix_line(line) for line in content.split("\n") if filter_line(line)))


def add_extern_attributes(content: str):
    for attribute in ReaderModule.__annotations__:
        if attribute not in ATTRIBUTES_OUTSIDE_OF_READER:
            content += f"\n let {attribute} = Reader.{attribute};;"
    for attribute in TagParserModule.__annotations__:
        content += f"\nlet {attribute} = Tag_Parser.{attribute};;"
    return content


def compile_and_save(content: str):
    with open("complied_content.ml", "w") as file:
        file.write(content)
    return ocaml.compile(content)


compile_module: Callable[[], TagParserModule] = pipe(compile_and_save, add_extern_attributes, fix_for_tests,
                                                  read_all_files)

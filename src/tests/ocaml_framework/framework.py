import functools
import inspect
import ocaml
import pathlib
from tests.ocaml_framework.compiler_data_types import *
import os

OCAML_FILES_IN_COMPILING_ORDER = ["src/pc_and_compiler.ml"]

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


MODULES_TO_FIX = ['Reader', 'Tag_Parser', 'Semantic_Analysis', 'Code_Generation']
def fix_line(line: str):
    for mod in MODULES_TO_FIX:
        if line.find(f"module {mod}") == 0:
            return f"module {mod} = struct"

    return line


def filter_line(line: str):
    return line.find("#use") != 0


def fix_for_tests(content: str) -> str:
    return "\n".join((fix_line(line) for line in content.split("\n") if filter_line(line)))


def add_extern_attributes(content: str):
    for attribute in ReaderModule.__annotations__:
        if attribute not in ATTRIBUTES_OUTSIDE_OF_READER:
            content += f"\nlet {attribute} = Reader.{attribute};;"
    for attribute in TagParserModule.__annotations__:
        content += f"\nlet {attribute} = Tag_Parser.{attribute};;"
    for attribute in SemanticAnalysisModule.__annotations__:
        content += f"\nlet {attribute} = Semantic_Analysis.{attribute};;"
    for attribute in CodeGenerationModule.__annotations__:
        content += f"\nlet {attribute} = Code_Generation.{attribute};;"
        
    return content


def compile_and_save(content: str):
    # with open("complied_content.ml", "w") as file:
    #     file.write(content)
    module = ocaml.compile(content)
    # os.chdir("..")
    return module


compile_module: Callable[[], any] = pipe(compile_and_save, add_extern_attributes, fix_for_tests,
                                                  read_all_files)

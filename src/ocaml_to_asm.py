import sys
import os
from pathlib import Path
from ast import literal_eval
import unittest



from tests.ocaml_framework.framework import compile_module
OCAML_SRC_FOLDER = 'ocaml_files/src'
OCAML_BUILD_FOLDER = 'ocaml_files/build'

OCAML_SRC_FILES = ['test1.scm']
OUTPUT_FILE = 'test1'


def main():
    print("Compiling ocaml...")
    module = compile_module()
    print("Done!")

    input_content = "\n".join([Path(OCAML_SRC_FOLDER, file).read_text() for file in OCAML_SRC_FILES])
    output_asm_file = Path(OCAML_BUILD_FOLDER, OUTPUT_FILE, ".asm").absolute().as_posix()
    print("Compiling scheme...")
    print(input_content)
    module.compile_scheme_string(output_asm_file, input_content)

if __name__ == "__main__":    
    main()
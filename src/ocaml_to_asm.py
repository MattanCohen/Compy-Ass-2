import sys
import os
from pathlib import Path
from ast import literal_eval
import unittest
import subprocess


from tests.ocaml_framework.framework import compile_module
SCHEME_SRC_FOLDER = 'src/scheme_files/src'
SCHEME_BUILD_FOLDER = 'src/scheme_files/build'

SCM_SRC_FILE = 'test1.scm'
OUTPUT_FILE = 'test1'
RUN_AFTER_COMPILING = True

def main():
    print("Compiling ocaml...")
    module = compile_module()
    input_scm_file = Path(SCHEME_SRC_FOLDER, SCM_SRC_FILE).absolute().as_posix()

    output_asm_file = Path(SCHEME_BUILD_FOLDER, f"{OUTPUT_FILE}.asm").absolute().as_posix()
    print("Input: ", input_scm_file)
    print("Input content:", "\n", Path(input_scm_file).read_text())
    print("Output: ", output_asm_file)


    os.chdir("src")
    print("Compiling scheme to asm...")
    module.compile_scheme_string(output_asm_file, input_scm_file)

    print("Compiling asm to ELF...")
    os.system(f"make {OUTPUT_FILE} -C scheme_files/build")

    if RUN_AFTER_COMPILING:
        print("Running compiled elf...")
        result = subprocess.run([f"./scheme_files/build/{OUTPUT_FILE}"], stdout=subprocess.PIPE)
        print(result.stdout)



if __name__ == "__main__":    
    main()
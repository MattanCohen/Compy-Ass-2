import pathlib
import zipfile

SOURCE_DIR = 'src'
SUBMISSION_ZIP_NAME = '207382581_316082577'
INPUT_OCAML_FILE = 'pc_and_compiler.ml'
COMPILER_OUTPUT_FILE = 'compiler.ml'
PC_OUTPUT_FILE = 'pc.ml'
OTHER_INPUT_FILES = ['epilogue.asm', 'init.scm', 'makefile', 'prologue-1.asm', 'prologue-2.asm', 'readme.txt']
OUTPUT_FILES = OTHER_INPUT_FILES + [PC_OUTPUT_FILE, COMPILER_OUTPUT_FILE]

def split_combined_file() -> tuple[str, str]:
    seperator = '(* pc.ml ends here *)'
    import_line = "#use 'pc.ml';;"
    combined_content = pathlib.Path(SOURCE_DIR, INPUT_OCAML_FILE).read_text()

    pc_ml_content = combined_content[:combined_content.find(seperator)]
    compiler_ml_content = import_line + combined_content[combined_content.find(seperator) + len(seperator):]

    return pc_ml_content, compiler_ml_content




def main():
    pc_ml_content, compiler_ml_content = split_combined_file()
    pc_ml_path = pathlib.Path(SOURCE_DIR, PC_OUTPUT_FILE)
    compiler_ml_path = pathlib.Path(SOURCE_DIR, COMPILER_OUTPUT_FILE)

    with pc_ml_path.open('w', encoding='utf8') as pc_ml:
        pc_ml.write(pc_ml_content)

    with compiler_ml_path.open('w', encoding='utf8') as compiler_ml:
        compiler_ml.write(compiler_ml_content)

    with zipfile.ZipFile(f'{SUBMISSION_ZIP_NAME}.zip', 'w') as zf:
        for input_file in OUTPUT_FILES:
            zf.write(pathlib.Path(SOURCE_DIR, input_file).absolute(), input_file)

    print(f'done! saved at: {SUBMISSION_ZIP_NAME}.zip')


if __name__ == '__main__':
    main()

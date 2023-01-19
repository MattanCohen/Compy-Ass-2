FILE?=test

PYHTON_COMMAND = /bin/python3
PYTHON_PATH = /mnt/c/CazeMattan/University/3rd year/5th semester/Compy/HW/Assignment 2/Compy-Ass-2/src/tests/tests/
PYHTON = ${PYHTON_COMMAND} "${PYTHON_PATH}${FILE}.py"

SCHEME_COMMAND = scheme
SCHEME = ${SCHEME_COMMAND} ${FILE}.scm

ASM_SRC_PATH = ./src/scheme_files/build/
ASM = ${ASM_SRC_PATH}${FILE}

MAYER_MAKE_PATH = src/scheme_files/build

echo:
	clear
	@echo how to use this makefile : 
	@echo -------------------------- 
	@echo use "make python" to run unit tests from ${FILE}.py 
	@echo use "make python FILE=_your_python_file_name_" to run unit tests 
	@echo 
	@echo use "make scheme" to run scheme on ${FILE}.scm 
	@echo use "make scheme FILE=_your_scm_file_name_" to run your file with scheme 
	@echo 
	@echo use "make assembly" to run the elf ${FILE}
	@echo use "make assembly FILE=_your_ELF_file_name_" to run your elf assembly 
	@echo 
	@echo use "make gdb-assembly" to run the elf ${FILE} with gdb
	@echo use "make gdb-assembly FILE=_your_ELF_file_name_" to run your elf with gdb


python:
	clear
	${PYHTON}

scheme:
	clear
	${SCHEME}

assembly:
	clear
	${ASM}

gdb-assembly:
	clear
	gdb ${ASM}

mayer:
	clear
	@echo 
	@echo use "make mayer" to use mayers make on asm file ${FILE}
	@echo use "make mayer FILE=_your_ELF_file_name_" to use mayers make on your asm file 
	make ${FILE} -C ${MAYER_MAKE_PATH}

nasm:
	rm -f test1.o
	nasm -f elf64 src/scheme_files/build/test1.asm -o test1.o
	ld test1.o -o test1

python-debug:
	clear
	/usr/bin/env /bin/python3 /home/opsidezi/.vscode-server/extensions/ms-python.python-2022.20.1/pythonFiles/lib/python/debugpy/adapter/../../debugpy/launcher 38571 -- /mnt/c/CazeMattan/University/3rd\ year/5th\ semester/Compy/HW/Assignment\ 2/Compy-Ass-2/tests/tests/test.py

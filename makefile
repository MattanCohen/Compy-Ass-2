FN?=apply.scm		#	apply.scm = default_value_if_not_set_in_environment

echo:
	@echo use "make python" to run unit tests 
	@echo use "make scheme FN=_your_scm_file_name_.scm" to run your file with scheme 

python:
	clear
	/bin/python3 "/mnt/c/CazeMattan/University/3rd year/5th semester/Compy/HW/Assignment 2/Compy-Ass-2/tests/tests/test.py"

python-debug:
	clear
	/usr/bin/env /bin/python3 /home/opsidezi/.vscode-server/extensions/ms-python.python-2022.20.1/pythonFiles/lib/python/debugpy/adapter/../../debugpy/launcher 38571 -- /mnt/c/CazeMattan/University/3rd\ year/5th\ semester/Compy/HW/Assignment\ 2/Compy-Ass-2/tests/tests/test.py


scheme:
	clear
	scheme ${FN}
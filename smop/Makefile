FLAGS=

check:
	python runtime.py $(FLAGS)
	#python test_runtime.py $(FLAGS)
	python test_matlabarray.py $(FLAGS)
	python test_sparsearray.py $(FLAGS)
	python test_lexer.py $(FLAGS)
	python main.py solver.m && python go.py | wc
	python test_runtime.py  && python main.py fastsolver.m && python go.py

FLAGS=

check:
	python runtime.py $(FLAGS)
	python test_runtime.py $(FLAGS)
	python test_matlabarray.py $(FLAGS)
	python test_sparsearray.py $(FLAGS)
	python test_lexer.py $(FLAGS)
	python main.py solver.m r8_random.m && python go.py | tail
	#python main.py fastsolver.m && python go.py

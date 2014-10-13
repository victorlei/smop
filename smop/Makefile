# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

OCTAVE = ~/octave-3.8.2/scripts
CYTHON = cython
PYTHON = python
FLAGS =
V = 2.7

go_so: solver.so
	$(PYTHON) go.py	

go_py: solver.py
	$(PYTHON) go.py

go_m: solver.m
	octave -q go.m

clean:
	rm -f a.* *.pyc solver.so solver.py runtime.so

solver.py: solver.m r8_random.m 
	$(PYTHON) main.py $^ -o $@

check:
	$(PYTHON) runtime.py $(FLAGS)
	$(PYTHON) test_runtime.py $(FLAGS)
	$(PYTHON) test_matlabarray.py $(FLAGS)
	$(PYTHON) test_sparsearray.py $(FLAGS)
	$(PYTHON) test_lexer.py $(FLAGS)
	$(PYTHON) main.py solver.m r8_random.m -o solver.py && python go.py
	find $(OCTAVE) -name \*.m | xargs python main.py -X legend.m | wc


%.c: %.pyx
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^


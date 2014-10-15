# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

OCTAVE = ~/octave-3.8.2/scripts
CYTHON = cython
PYTHON = python
FLAGS = --syntax-errors=stft.m
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
	$(PYTHON) runtime.py 
	$(PYTHON) test_runtime.py
	$(PYTHON) test_matlabarray.py
	$(PYTHON) test_sparsearray.py
	$(PYTHON) test_lexer.py 
	$(PYTHON) main.py $(FLAGS) solver.m r8_random.m -o solver.py && python go.py
      
test:
	find $(OCTAVE) -name \*.m | xargs python main.py $(FLAGS)


%.c: %.pyx
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^


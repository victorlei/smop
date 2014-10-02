FLAGS=
PYTHON=/home/lei/pypy-2.4.0-linux/bin/pypy

timer_so: a.so
	rm -f a.py a.pyc
	$(PYTHON) go.py	

timer_py: a.py
	$(PYTHON) go.py

clean:
	rm -f a.* *.pyc

a.py: solver.m r8_random.m 
	$(PYTHON) main.py $^

check:
	$(PYTHON) runtime.py $(FLAGS)
	$(PYTHON) test_runtime.py $(FLAGS)
	$(PYTHON) test_matlabarray.py $(FLAGS)
	$(PYTHON) test_sparsearray.py $(FLAGS)
	$(PYTHON) test_lexer.py $(FLAGS)
	$(PYTHON) main.py solver.m r8_random.m && $(PYTHON) go.py | tail
	#$(PYTHON) main.py fastsolver.m && python go.py

%.c: %.pyx
	cython -a $^

%.so: %.c
	gcc -I /usr/include/python2.7 -O2 -shared -o $@ $^


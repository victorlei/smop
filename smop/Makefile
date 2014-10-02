FLAGS=

timer_so: a.so
	rm -f a.py a.pyc
	python go.py	

timer_py: a.py
	python go.py

clean:
	rm -f a.* *.pyc

a.py: solver.m r8_random.m 
	python main.py $^

check:
	python runtime.py $(FLAGS)
	python test_runtime.py $(FLAGS)
	python test_matlabarray.py $(FLAGS)
	python test_sparsearray.py $(FLAGS)
	python test_lexer.py $(FLAGS)
	python main.py solver.m r8_random.m && python go.py | tail
	#python main.py fastsolver.m && python go.py

%.c: %.pyx
	cython -a $^

%.so: %.c
	gcc -I /usr/include/python2.7 -O2 -shared -o $@ $^


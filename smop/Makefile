FLAGS=

go_so: solver.so
	python go.py	

go_py: solver.py
	python go.py

go_m: solver.m
	octave -q go.m

clean:
	rm -f a.* *.pyc solver.so solver.py

solver.py: solver.m r8_random.m 
	python main.py $^ -o $@

check:
	#python runtime.py $(FLAGS)
	python test_runtime.py $(FLAGS)
	python test_matlabarray.py $(FLAGS)
	python test_sparsearray.py $(FLAGS)
	python test_lexer.py $(FLAGS)
	python main.py solver.m r8_random.m -o solver.py && python go.py
	#python main.py fastsolver.m && python go.py

%.c: %.pyx
	cython -a $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python2.7 -O2 -shared -o $@ $^


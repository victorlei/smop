
# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

VPATH = $(SCRIPTS)/strings $(SCRIPTS)/specfun
OCTAVE = /home/lei/octave-4.0.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
XFILES = -x inputParser.m,quadgk.m,quadl.m,triplequad.m,dblquad.m
FLAGS  = 
V = 2.7
ALL = \*.m

factorial.py: factorial.m
	$(PYTHON) main.py -v -loctave $^ -o-  $(FLAGS) 
all: mybench.py 
	$(PYTHON) -c "import mybench ; mybench.mybench()"

#octave.py: primes.m interp2.m meshgrid.m repmat.m
#	$(PYTHON) main.py --callgraph $(FLAGS) $^ -r core -o $@
#	dot -Tpdf -o G.pdf G.dot

#test_primes: octave.py
#	$(PYTHON) -c "import octave ; octave.primes(1000)"

octave.py:
	find $(SCRIPTS)/general $(SCRIPTS)/specfun -name \*.m | xargs python main.py -o $@  $(FLAGS) $(XFILES)


clean:
	rm -f a.* *.pyc solver.so solver.py octave.so

solver.py: solver.m #r8_random.m octave.py
	$(PYTHON) main.py $^ -o $@ 

check: solver.py 
	$(PYTHON) test_core.py
	$(PYTHON) test_matlabarray.py
	$(PYTHON) test_sparsearray.py
	$(PYTHON) test_lexer.py 
	$(PYTHON) test_solver.py
#	$(PYTHON) test_primes.py

#$(PYTHON) main.py $(FLAGS) -r core solver.m r8_random.m -o solver.py && python go.py
#test:
#	find $(SCRIPTS) -name $(ALL) | xargs python main.py $(FLAGS)

%.c: %.py
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^

%.py: %.m
	#smop $(FLAGS) -o $@ $^
	$(PYTHON) main.py -o $@ $^
	$(PYTHON) $@

%.pdf: %.dot
	dot -Tpdf -o $@ $^

%.pdf: %.rst
	rst2pdf $^

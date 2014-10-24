# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

VPATH = $(SCRIPTS)/general:$(SCRIPTS)/specfun:$(SCRIPTS)/plot/util
OCTAVE = /home/lei/octave-3.8.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
#FLAGS = -S stft.m,datenum.m -X orderfields.m,legend.m,pack.m,unpack.m,__unimplemented__.m,assert.m,optimset.m
#FLAGS = -X interp1.m,interpn.m,nargchk.m,nargoutchk.m,profexplore.m,profshow.m,quadgk.m,quadl.m
FLAGS = -Xquadgk,quadl,lcm,adaptlobstp,spline,ppval,ellipke
V = 2.7
ALL = \*.m

all: mybench.py 
	$(PYTHON) -c "import mybench ; mybench.mybench()"

octave.py: primes.m interp2.m meshgrid.m repmat.m
	$(PYTHON) main.py $^ -r core -o $@

#octave.py:
#	find $(SCRIPTS)/general $(SCRIPTS)/specfun -name \*.m | xargs smop -v -o $@ -r core $(FLAGS)


go_so: solver.so
	$(PYTHON) go.py	

go_py: solver.py
	$(PYTHON) go.py

go_m: solver.m
	octave -q go.m

clean:
	rm -f a.* *.pyc solver.so solver.py octave.so

solver.py: solver.m r8_random.m 
	$(PYTHON) main.py $^ -o $@

check:
	$(PYTHON) octave.py 
	$(PYTHON) test_core.py
	$(PYTHON) test_matlabarray.py
	$(PYTHON) test_sparsearray.py
	-$(PYTHON) test_lexer.py 
	$(PYTHON) main.py $(FLAGS) solver.m r8_random.m -o solver.py && python go.py
      
test:
	find $(SCRIPTS) -name $(ALL) | xargs python main.py $(FLAGS)

%.c: %.py
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^

%.py: %.m
	#smop $(FLAGS) -o $@ $^
	$(PYTHON) main.py -o $@ $^ -r core -r octave
	$(PYTHON) $@

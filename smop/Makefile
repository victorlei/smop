# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

OCTAVE = /home/lei/octave-3.8.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
#FLAGS = -S stft.m,datenum.m -X orderfields.m,legend.m,pack.m,unpack.m,__unimplemented__.m,assert.m,optimset.m
#FLAGS = -X interp1.m,interpn.m,nargchk.m,nargoutchk.m,profexplore.m,profshow.m,quadgk.m,quadl.m
FLAGS = -Xquadgk.m,quadl.m,lcm.m,adaptlobstp.m,spline.m,ppval.m
V = 2.7
ALL = \*.m

all: mybench.py 
	$(PYTHON) -c "import mybench ; mybench.mybench()"

octave.py:
	find $(SCRIPTS)/general $(SCRIPTS)/specfun -name \*.m | xargs smop -v -o $@ -r core $(FLAGS)

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
	find $(SCRIPTS) -name $(ALL) | xargs python main.py $(FLAGS)

%.c: %.pyx
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^

%.py: %.m
	smop $(FLAGS) -o $@ $^
	#$(PYTHON) main.py -o $@ $^
	$(PYTHON) $@

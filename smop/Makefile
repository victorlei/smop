# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

OCTAVE = /home/lei/octave-4.0.2
SCRIPTS = $(OCTAVE)/scripts/specfun

CYTHON = cython
PYTHON = python
XFILES = -x inputParser.m,dblquad.m,triplequad.m

FLAGS  = 
MYFLAGS=

V = 2.7

all:
	make -B FLAGS=     liboctave.py
foo:
	make -B FLAGS=-C   liboctave.py
	make -B FLAGS=-N   liboctave.py
	make -B FLAGS=-T   liboctave.py
	make -B FLAGS=-CN  liboctave.py
	make -B FLAGS=-TN  liboctave.py
	make -B FLAGS=-CT  liboctave.py
	make -B FLAGS=-CTN liboctave.py

liboctave.py:
	find $(SCRIPTS) -name \*.m | xargs $(PYTHON) main.py --verbose -o $@ $(MYFLAGS) $(FLAGS) $(XFILES) $^
	#$(PYTHON) $@

clean:
	rm -f a.* *.pyc solver.so solver.py octave.so

regress:
	make | sort -u | wc
%.c: %.py
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^

%.py: %.m
	$(PYTHON) main.py -o $@ $^
	#$(PYTHON) $@

%.pdf: %.dot
	dot -Tpdf -o $@ $^

%.pdf: %.rst
	rst2pdf $^

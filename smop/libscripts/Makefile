# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4
VPATH  = $(shell find $(SCRIPTS) -type d)
OCTAVE = /home/lei/octave-4.0.2
SCRIPTS= $(OCTAVE)/scripts
PYFILES= $(sort $(subst .m,.py,$(notdir $(MFILES) )))
MFILES = $(shell find $(SCRIPTS) -name \*.m -type f)
CYTHON = cython
PYTHON = python
CAT    = cat
XFILES = -x fnmatch.m
FLAGS  = -v
SMOP   = $(PYTHON) ../main.py

V = 2.7

all: 
	$(SMOP) $(FLAGS) $(XFILES) $(MFILES)
	rm -f libscripts.py
	$(CAT) *.py > libscripts.py

regress: 
	$(SMOP) $(FLAGS) $(MFILES) | grep Error | wc

.PHONY: all pyfiles clean

clean:
	rm -f libscripts.py *.pyc *.py

%.py: %.m
	$(SMOP) $^ $(FLAGS) $(XFILES)

%.pyc: %.py
	$(PYTHON) -m py_compile $@

.DELETE_ON_ERROR:


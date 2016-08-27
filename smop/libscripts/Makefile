# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4
VPATH  = $(shell find $(SCRIPTS) -type d)
OCTAVE = /home/lei/octave-4.0.2
SCRIPTS= $(OCTAVE)/scripts
PYFILES= $(sort $(subst .m,.py,$(notdir $(MFILES) )))
MFILES = $(shell find $(SCRIPTS) -name \*.m -type f)
CYTHON = cython
PYTHON = python
XFILES = 
FLAGS  = 
SMOP   = $(PYTHON) ../main.py

V = 2.7

all: $(PYFILES)
	grep __main__ libscripts.py | wc -l

clean:
	rm -f libscripts.py *.pyc *.py

%.py: %.m
	$(SMOP) $^ $(FLAGS)
	($(PYTHON) $@ && cat $@ >> libscripts.py)



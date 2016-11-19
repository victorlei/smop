# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

OCTAVE = /home/lei/octave-4.0.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
XFILES = -x inputParser

#m,uiputfile.m,uigetfile.m,qmr.m

#quadgk.m,quadl.m,triplequad.m,dblquad.m
#installed_packages.m,stft.m,legend.m,__plt_get_axis_arg__.m,rotate.m,print.m,__ghostscript__.m,__gnuplot_print__.m,set.m,edit.m,what.m,usejava.m,javachk.m,__go_draw_axes__.m,interp3.m,interp2.m,interpn.m,randi.m,interp1.m,spdiags.m,importdata.m,stemleaf.m
FLAGS  = 
MYFLAGS=

V = 2.7

all:
	make -B FLAGS=     liboctave.py | wc
foo:
	make -B FLAGS=-C   liboctave.py | wc
	make -B FLAGS=-N   liboctave.py | wc
	make -B FLAGS=-T   liboctave.py | wc
	make -B FLAGS=-CN  liboctave.py | wc
	make -B FLAGS=-TN  liboctave.py | wc
	make -B FLAGS=-CT  liboctave.py | wc
	make -B FLAGS=-CTN liboctave.py | wc

liboctave.py:
	find $(SCRIPTS) -name \*.m | xargs $(PYTHON) main.py -o $@ $(MYFLAGS) $(FLAGS) $(XFILES) $^
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
	$(PYTHON) $@

%.pdf: %.dot
	dot -Tpdf -o $@ $^

%.pdf: %.rst
	rst2pdf $^

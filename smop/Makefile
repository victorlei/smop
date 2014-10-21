# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

VPATH = $(SCRIPTS)/general
OCTAVE = /home/lei/octave-3.8.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
#FLAGS = -S stft.m,datenum.m -X orderfields.m,legend.m,pack.m,unpack.m,__unimplemented__.m,assert.m,optimset.m
FLAGS = -X interp1.m,interpn.m,nargchk.m,nargoutchk.m,profexplore.m,profshow.m,quadgk.m,quadl.m
V = 2.7
ALL = \*.m
GENERAL = accumarray.m accumdim.m bicubic.m bincoeff.m\
    bitcmp.m bitget.m bitset.m blkdiag.m cart2pol.m cart2sph.m\
    cell2mat.m celldisp.m chop.m circshift.m common_size.m cplxpair.m\
    cumtrapz.m curl.m dblquad.m deal.m del2.m display.m divergence.m\
    fieldnames.m flipdim.m fliplr.m flipud.m gradient.m idivide.m\
    int2str.m interp1.m interp2.m interp3.m interpft.m interpn.m\
    isa.m iscolumn.m isdir.m isequal.m isequaln.m isrow.m isscalar.m\
    issquare.m isvector.m loadobj.m logspace.m methods.m nargchk.m\
    narginchk.m nargoutchk.m nextpow2.m nthargout.m num2str.m pol2cart.m\
    polyarea.m postpad.m prepad.m profexplore.m profile.m profshow.m\
    quadgk.m quadl.m quadv.m randi.m rat.m repmat.m rot90.m rotdim.m\
    saveobj.m shiftdim.m shift.m sortrows.m sph2cart.m structfun.m\
    subsindex.m trapz.m triplequad.m

all: mybench.py 
	$(PYTHON) -c "import mybench ; mybench.mybench()"

general.py: $(GENERAL)
	$(PYTHON) main.py $^ -o $@ -v $(FLAGS)

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

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
GENERAL = accumarray.py accumdim.py bicubic.py bincoeff.py\
    bitcmp.py bitget.py bitset.py blkdiag.py cart2pol.py cart2sph.py\
    cell2mat.py celldisp.py chop.py circshift.py common_size.py cplxpair.py\
    cumtrapz.py curl.py dblquad.py deal.py del2.py display.py divergence.py\
    fieldnames.py flipdim.py fliplr.py flipud.py gradient.py idivide.py\
    int2str.py interp1.py interp2.py interp3.py interpft.py interpn.py\
    isa.py iscolumn.py isdir.py isequal.py isequaln.py isrow.py isscalar.py\
    issquare.py isvector.py loadobj.py logspace.py methods.py nargchk.py\
    narginchk.py nargoutchk.py nextpow2.py nthargout.py num2str.py pol2cart.py\
    polyarea.py postpad.py prepad.py profexplore.py profile.py profshow.py\
    quadgk.py quadl.py quadv.py randi.py rat.py repmat.py rot90.py rotdim.py\
    saveobj.py shiftdim.py shift.py sortrows.py sph2cart.py structfun.py\
    subsindex.py trapz.py triplequad.py

all: mybench.py runtime.py
	$(PYTHON) -c "import mybench ; mybench.mybench()"

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

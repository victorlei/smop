# for py3: make PYTHON=python3 CYTHON="cython -3" V=3.4

VPATH = .:$(SCRIPTS)/general:$(SCRIPTS)/specfun
OCTAVE = /home/lei/octave-3.8.2
SCRIPTS = $(OCTAVE)/scripts

CYTHON = cython
PYTHON = python
FLAGS = -Xquadgk.m,quadl.m,lcm.m
V = 2.7
SMOP = smop
SPECFUN = bessel.py betaln.py beta.py ellipke.py expint.py factorial.py\
    factor.py isprime.py lcm.py legendre.py nchoosek.py\
    nthroot.py perms.py pow2.py primes.py reallog.py realpow.py\
    realsqrt.py

GENERAL = accumarray.py accumdim.py bicubic.py bincoeff.py\
    bitcmp.py bitget.py bitset.py blkdiag.py cart2pol.py cart2sph.py\
    cell2mat.py celldisp.py chop.py circshift.py common_size.py\
    cplxpair.py cumtrapz.py curl.py dblquad.py deal.py del2.py display.py\
    divergence.py fieldnames.py flipdim.py fliplr.py flipud.py gradient.py\
    idivide.py int2str.py interp1.py interp2.py interp3.py interpft.py\
    interpn.py isa.py iscolumn.py isdir.py isequal.py isequaln.py\
    isrow.py isscalar.py issquare.py isvector.py loadobj.py logspace.py\
    methods.py nargchk.py narginchk.py nargoutchk.py nextpow2.py\
    nthargout.py num2str.py pol2cart.py polyarea.py postpad.py prepad.py\
    profexplore.py profile.py profshow.py quadgk.py quadl.py quadv.py\
    randi.py rat.py repmat.py rot90.py rotdim.py saveobj.py shiftdim.py\
    shift.py sortrows.py sph2cart.py structfun.py subsindex.py trapz.py\
    triplequad.py

all: $(GENERAL) $(SPECFUN) core.py r8_random.py

clean:
	rm -f a.* *.pyc *.so $(GENERAL) $(SPECFUN) r8_random.py

%.c: %.pyx
	$(CYTHON) $^

%.so: %.c
	gcc -Wno-cpp -I /usr/include/python$V -O2 -shared -o $@ $^

%.py: %.m
	smop $(FLAGS) -o $@ $^

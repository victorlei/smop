FCN_FILE_DIRS += specfun

specfun_FCN_FILES = \
  specfun/bessel.m \
  specfun/beta.m \
  specfun/betaln.m \
  specfun/ellipke.m \
  specfun/expint.m \
  specfun/factor.m \
  specfun/factorial.m \
  specfun/isprime.m \
  specfun/lcm.m \
  specfun/legendre.m \
  specfun/nchoosek.m \
  specfun/nthroot.m \
  specfun/perms.m \
  specfun/pow2.m \
  specfun/primes.m \
  specfun/reallog.m \
  specfun/realpow.m \
  specfun/realsqrt.m

FCN_FILES += $(specfun_FCN_FILES)

PKG_ADD_FILES += specfun/PKG_ADD

DIRSTAMP_FILES += specfun/$(octave_dirstamp)

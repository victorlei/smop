EXTRA_DIST += cruft/mkf77def.in

nodist_cruft_libcruft_la_SOURCES =

cruft_libcruft_la_FFLAGS = $(F77_INTEGER_8_FLAG)

cruft_libcruft_la_DEPENDENCIES = cruft/cruft.def

CRUFT_INC =

CRUFT_SOURCES =

include cruft/amos/module.mk
include cruft/blas-xtra/module.mk
include cruft/daspk/module.mk
include cruft/dasrt/module.mk
include cruft/dassl/module.mk
include cruft/Faddeeva/module.mk
include cruft/fftpack/module.mk
include cruft/lapack-xtra/module.mk
include cruft/misc/module.mk
include cruft/odepack/module.mk
include cruft/ordered-qz/module.mk
include cruft/quadpack/module.mk
include cruft/ranlib/module.mk
include cruft/slatec-err/module.mk
include cruft/slatec-fn/module.mk

define gen-cruft-def
  rm -f $@-t $@ && \
  $(SHELL) cruft/mkf77def $(srcdir) $(cruft_libcruft_la_SOURCES) > $@-t && \
  mv $@-t $@
endef

## Special rules for files which must be built before compilation
cruft/cruft.def: $(cruft_libcruft_la_SOURCES) cruft/mkf77def
	$(AM_V_GEN)$(gen-cruft-def)

DISTCLEANFILES += \
  cruft/cruft.def \
  cruft/mkf77def \
  cruft/ranlib/ranlib.def \
  $(nodist_cruft_libcruft_la_SOURCES)

noinst_LTLIBRARIES += cruft/libcruft.la

cruft_libcruft_la_SOURCES = $(CRUFT_SOURCES)
cruft_libcruft_la_CPPFLAGS = \
  $(liboctave_la_CPPFLAGS)

liboctave_la_LIBADD += cruft/libcruft.la

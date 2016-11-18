## DO NOT EDIT -- generated from module-files by config-module.awk

EXTRA_DIST += \
  dldfcn/config-module.sh \
  dldfcn/config-module.awk \
  dldfcn/module-files \
  dldfcn/oct-qhull.h

DLDFCN_SRC = \
  dldfcn/__delaunayn__.cc \
  dldfcn/__eigs__.cc \
  dldfcn/__fltk_uigetfile__.cc \
  dldfcn/__glpk__.cc \
  dldfcn/__init_fltk__.cc \
  dldfcn/__init_gnuplot__.cc \
  dldfcn/__magick_read__.cc \
  dldfcn/__osmesa_print__.cc \
  dldfcn/__voronoi__.cc \
  dldfcn/amd.cc \
  dldfcn/audiodevinfo.cc \
  dldfcn/audioread.cc \
  dldfcn/ccolamd.cc \
  dldfcn/chol.cc \
  dldfcn/colamd.cc \
  dldfcn/convhulln.cc \
  dldfcn/dmperm.cc \
  dldfcn/fftw.cc \
  dldfcn/qr.cc \
  dldfcn/symbfact.cc \
  dldfcn/symrcm.cc

DLDFCN_LIBS = $(DLDFCN_SRC:.cc=.la)

if AMCOND_ENABLE_DYNAMIC_LINKING

octlib_LTLIBRARIES += $(DLDFCN_LIBS)

## Use stamp files to avoid problems with checking timestamps
## of symbolic links

%.oct : %.la
	$(AM_V_GEN)$(INSTALL_PROGRAM) dldfcn/.libs/$(shell $(SED) -n -e "s/dlname='\([^']*\)'/\1/p" < $<) $@

else

noinst_LTLIBRARIES += $(DLDFCN_LIBS)

endif

dldfcn___delaunayn___la_SOURCES = dldfcn/__delaunayn__.cc
dldfcn/__delaunayn__.df: CPPFLAGS += $(QHULL_CPPFLAGS)
dldfcn___delaunayn___la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
dldfcn___delaunayn___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___delaunayn___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(QHULL_LIBS) $(OCT_LINK_DEPS)

dldfcn___eigs___la_SOURCES = dldfcn/__eigs__.cc
dldfcn/__eigs__.df: CPPFLAGS += $(ARPACK_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn___eigs___la_CPPFLAGS = $(AM_CPPFLAGS) $(ARPACK_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn___eigs___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(ARPACK_LDFLAGS) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn___eigs___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(ARPACK_LIBS) $(SPARSE_XLIBS) $(LAPACK_LIBS) $(BLAS_LIBS) $(OCT_LINK_DEPS)

dldfcn___fltk_uigetfile___la_SOURCES = dldfcn/__fltk_uigetfile__.cc
dldfcn/__fltk_uigetfile__.df: CPPFLAGS += $(FLTK_CPPFLAGS) $(FT2_CPPFLAGS)
dldfcn___fltk_uigetfile___la_CPPFLAGS = $(AM_CPPFLAGS) $(FLTK_CPPFLAGS) $(FT2_CPPFLAGS)
dldfcn___fltk_uigetfile___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(FLTK_LDFLAGS) $(FT2_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___fltk_uigetfile___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(FLTK_LIBS) $(FT2_LIBS) $(OCT_LINK_DEPS)

dldfcn___glpk___la_SOURCES = dldfcn/__glpk__.cc
dldfcn/__glpk__.df: CPPFLAGS += $(GLPK_CPPFLAGS)
dldfcn___glpk___la_CPPFLAGS = $(AM_CPPFLAGS) $(GLPK_CPPFLAGS)
dldfcn___glpk___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(GLPK_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___glpk___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(GLPK_LIBS) $(OCT_LINK_DEPS)

dldfcn___init_fltk___la_SOURCES = dldfcn/__init_fltk__.cc
dldfcn/__init_fltk__.df: CPPFLAGS += $(FLTK_CPPFLAGS) $(FT2_CPPFLAGS) $(FONTCONFIG_CPPFLAGS)
dldfcn___init_fltk___la_CPPFLAGS = $(AM_CPPFLAGS) $(FLTK_CPPFLAGS) $(FT2_CPPFLAGS) $(FONTCONFIG_CPPFLAGS)
dldfcn___init_fltk___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(FLTK_LDFLAGS) $(FT2_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___init_fltk___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(FLTK_LIBS) $(FT2_LIBS) $(OPENGL_LIBS) $(OCT_LINK_DEPS)

dldfcn___init_gnuplot___la_SOURCES = dldfcn/__init_gnuplot__.cc
dldfcn/__init_gnuplot__.df: CPPFLAGS += $(FT2_CPPFLAGS) $(FONTCONFIG_CPPFLAGS)
dldfcn___init_gnuplot___la_CPPFLAGS = $(AM_CPPFLAGS) $(FT2_CPPFLAGS) $(FONTCONFIG_CPPFLAGS)
dldfcn___init_gnuplot___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG)  $(OCT_LINK_OPTS)
dldfcn___init_gnuplot___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la  $(OCT_LINK_DEPS)

dldfcn___magick_read___la_SOURCES = dldfcn/__magick_read__.cc
dldfcn/__magick_read__.df: CPPFLAGS += $(MAGICK_CPPFLAGS)
dldfcn___magick_read___la_CPPFLAGS = $(AM_CPPFLAGS) $(MAGICK_CPPFLAGS)
dldfcn___magick_read___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(MAGICK_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___magick_read___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(MAGICK_LIBS) $(OCT_LINK_DEPS)

dldfcn___osmesa_print___la_SOURCES = dldfcn/__osmesa_print__.cc
dldfcn/__osmesa_print__.df: CPPFLAGS += $(OSMESA_CPPFLAGS) $(FT2_CPPFLAGS)
dldfcn___osmesa_print___la_CPPFLAGS = $(AM_CPPFLAGS) $(OSMESA_CPPFLAGS) $(FT2_CPPFLAGS)
dldfcn___osmesa_print___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(OSMESA_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___osmesa_print___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(OSMESA_LIBS) $(OCT_LINK_DEPS)

dldfcn___voronoi___la_SOURCES = dldfcn/__voronoi__.cc
dldfcn/__voronoi__.df: CPPFLAGS += $(QHULL_CPPFLAGS)
dldfcn___voronoi___la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
dldfcn___voronoi___la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn___voronoi___la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(QHULL_LIBS) $(OCT_LINK_DEPS)

dldfcn_amd_la_SOURCES = dldfcn/amd.cc
dldfcn/amd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_amd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_amd_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_amd_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_audiodevinfo_la_SOURCES = dldfcn/audiodevinfo.cc
dldfcn/audiodevinfo.df: CPPFLAGS += $(PORTAUDIO_CPPFLAGS)
dldfcn_audiodevinfo_la_CPPFLAGS = $(AM_CPPFLAGS) $(PORTAUDIO_CPPFLAGS)
dldfcn_audiodevinfo_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(PORTAUDIO_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn_audiodevinfo_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(PORTAUDIO_LIBS) $(OCT_LINK_DEPS)

dldfcn_audioread_la_SOURCES = dldfcn/audioread.cc
dldfcn/audioread.df: CPPFLAGS += $(SNDFILE_CPPFLAGS)
dldfcn_audioread_la_CPPFLAGS = $(AM_CPPFLAGS) $(SNDFILE_CPPFLAGS)
dldfcn_audioread_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SNDFILE_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn_audioread_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SNDFILE_LIBS) $(OCT_LINK_DEPS)

dldfcn_ccolamd_la_SOURCES = dldfcn/ccolamd.cc
dldfcn/ccolamd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_ccolamd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_ccolamd_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_ccolamd_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_chol_la_SOURCES = dldfcn/chol.cc
dldfcn/chol.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_chol_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_chol_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_chol_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_colamd_la_SOURCES = dldfcn/colamd.cc
dldfcn/colamd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_colamd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_colamd_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_colamd_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_convhulln_la_SOURCES = dldfcn/convhulln.cc
dldfcn/convhulln.df: CPPFLAGS += $(QHULL_CPPFLAGS)
dldfcn_convhulln_la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
dldfcn_convhulln_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
dldfcn_convhulln_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(QHULL_LIBS) $(OCT_LINK_DEPS)

dldfcn_dmperm_la_SOURCES = dldfcn/dmperm.cc
dldfcn/dmperm.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_dmperm_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_dmperm_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_dmperm_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_fftw_la_SOURCES = dldfcn/fftw.cc
dldfcn/fftw.df: CPPFLAGS += $(FFTW_XCPPFLAGS)
dldfcn_fftw_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFTW_XCPPFLAGS)
dldfcn_fftw_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(FFTW_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_fftw_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(FFTW_XLIBS) $(OCT_LINK_DEPS)

dldfcn_qr_la_SOURCES = dldfcn/qr.cc
dldfcn/qr.df: CPPFLAGS += $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_qr_la_CPPFLAGS = $(AM_CPPFLAGS) $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_qr_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(QRUPDATE_LDFLAGS) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_qr_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(QRUPDATE_LIBS) $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_symbfact_la_SOURCES = dldfcn/symbfact.cc
dldfcn/symbfact.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_symbfact_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_symbfact_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_symbfact_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

dldfcn_symrcm_la_SOURCES = dldfcn/symrcm.cc
dldfcn/symrcm.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
dldfcn_symrcm_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
dldfcn_symrcm_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) $(SPARSE_XLDFLAGS) $(OCT_LINK_OPTS)
dldfcn_symrcm_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la $(SPARSE_XLIBS) $(OCT_LINK_DEPS)

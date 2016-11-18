EXTRA_DIST += \
  numeric/module.mk \
  $(OPT_IN)

OPT_INC = \
  numeric/DASPK-opts.h \
  numeric/DASRT-opts.h \
  numeric/DASSL-opts.h \
  numeric/LSODE-opts.h \
  numeric/Quad-opts.h

OPT_IN = $(OPT_INC:.h=.in)

NUMERIC_INC = \
  numeric/base-aepbal.h \
  numeric/base-dae.h \
  numeric/base-de.h \
  numeric/base-lu.h \
  numeric/base-min.h \
  numeric/base-qr.h \
  numeric/bsxfun-decl.h \
  numeric/bsxfun.h \
  numeric/CmplxAEPBAL.h \
  numeric/CmplxCHOL.h \
  numeric/CmplxGEPBAL.h \
  numeric/CmplxHESS.h \
  numeric/CmplxLU.h \
  numeric/CmplxQR.h \
  numeric/CmplxQRP.h \
  numeric/CmplxSCHUR.h \
  numeric/CmplxSVD.h \
  numeric/CollocWt.h \
  numeric/DAEFunc.h \
  numeric/DAE.h \
  numeric/DAERTFunc.h \
  numeric/DAERT.h \
  numeric/DASPK.h \
  numeric/DASRT.h \
  numeric/DASSL.h \
  numeric/dbleAEPBAL.h \
  numeric/dbleCHOL.h \
  numeric/dbleGEPBAL.h \
  numeric/dbleHESS.h \
  numeric/dbleLU.h \
  numeric/dbleQR.h \
  numeric/dbleQRP.h \
  numeric/dbleSCHUR.h \
  numeric/dbleSVD.h \
  numeric/DET.h \
  numeric/EIG.h \
  numeric/fCmplxAEPBAL.h \
  numeric/fCmplxCHOL.h \
  numeric/fCmplxGEPBAL.h \
  numeric/fCmplxHESS.h \
  numeric/fCmplxLU.h \
  numeric/fCmplxQR.h \
  numeric/fCmplxQRP.h \
  numeric/fCmplxSCHUR.h \
  numeric/fCmplxSVD.h \
  numeric/fEIG.h \
  numeric/floatAEPBAL.h \
  numeric/floatCHOL.h \
  numeric/floatGEPBAL.h \
  numeric/floatHESS.h \
  numeric/floatLU.h \
  numeric/floatQR.h \
  numeric/floatQRP.h \
  numeric/floatSCHUR.h \
  numeric/floatSVD.h \
  numeric/lo-mappers.h \
  numeric/lo-specfun.h \
  numeric/LSODE.h \
  numeric/oct-convn.h \
  numeric/oct-fftw.h \
  numeric/oct-norm.h \
  numeric/oct-rand.h \
  numeric/oct-spparms.h \
  numeric/ODEFunc.h \
  numeric/ODE.h \
  numeric/ODESFunc.h \
  numeric/ODES.h \
  numeric/Quad.h \
  numeric/randgamma.h \
  numeric/randmtzig.h \
  numeric/randpoisson.h \
  numeric/sparse-base-chol.h \
  numeric/sparse-base-lu.h \
  numeric/SparseCmplxCHOL.h \
  numeric/SparseCmplxLU.h \
  numeric/SparseCmplxQR.h \
  numeric/SparsedbleCHOL.h \
  numeric/SparsedbleLU.h \
  numeric/SparseQR.h

NUMERIC_C_SRC = \
  numeric/randgamma.c \
  numeric/randmtzig.c \
  numeric/randpoisson.c

NUMERIC_SRC = \
  numeric/CmplxAEPBAL.cc \
  numeric/CmplxCHOL.cc \
  numeric/CmplxGEPBAL.cc \
  numeric/CmplxHESS.cc \
  numeric/CmplxLU.cc \
  numeric/CmplxQR.cc \
  numeric/CmplxQRP.cc \
  numeric/CmplxSCHUR.cc \
  numeric/CmplxSVD.cc \
  numeric/CollocWt.cc \
  numeric/DASPK.cc \
  numeric/DASRT.cc \
  numeric/DASSL.cc \
  numeric/dbleAEPBAL.cc \
  numeric/dbleCHOL.cc \
  numeric/dbleGEPBAL.cc \
  numeric/dbleHESS.cc \
  numeric/dbleLU.cc \
  numeric/dbleQR.cc \
  numeric/dbleQRP.cc \
  numeric/dbleSCHUR.cc \
  numeric/dbleSVD.cc \
  numeric/EIG.cc \
  numeric/fCmplxAEPBAL.cc \
  numeric/fCmplxCHOL.cc \
  numeric/fCmplxGEPBAL.cc \
  numeric/fCmplxHESS.cc \
  numeric/fCmplxLU.cc \
  numeric/fCmplxQR.cc \
  numeric/fCmplxQRP.cc \
  numeric/fCmplxSCHUR.cc \
  numeric/fCmplxSVD.cc \
  numeric/fEIG.cc \
  numeric/floatAEPBAL.cc \
  numeric/floatCHOL.cc \
  numeric/floatGEPBAL.cc \
  numeric/floatHESS.cc \
  numeric/floatLU.cc \
  numeric/floatQR.cc \
  numeric/floatQRP.cc \
  numeric/floatSCHUR.cc \
  numeric/floatSVD.cc \
  numeric/lo-mappers.cc \
  numeric/lo-specfun.cc \
  numeric/LSODE.cc \
  numeric/oct-convn.cc \
  numeric/oct-fftw.cc \
  numeric/oct-norm.cc \
  numeric/oct-rand.cc \
  numeric/oct-spparms.cc \
  numeric/ODES.cc \
  numeric/Quad.cc \
  numeric/SparseCmplxCHOL.cc \
  numeric/SparseCmplxLU.cc \
  numeric/SparseCmplxQR.cc \
  numeric/SparsedbleCHOL.cc \
  numeric/SparsedbleLU.cc \
  numeric/SparseQR.cc \
  $(NUMERIC_C_SRC)

TEMPLATE_SRC += \
  numeric/base-lu.cc \
  numeric/base-qr.cc \
  numeric/bsxfun-defs.cc \
  numeric/eigs-base.cc \
  numeric/sparse-base-chol.cc \
  numeric/sparse-base-lu.cc \
  numeric/sparse-dmsolve.cc

## Special rules for sources which must be built before rest of compilation.
$(OPT_INC) : %.h : %.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(top_srcdir)/build-aux/mk-opts.pl --opt-class-header $< > $@-t && \
	mv $@-t $@

$(OPT_INC) : $(top_srcdir)/build-aux/mk-opts.pl

noinst_LTLIBRARIES += numeric/libnumeric.la

numeric_libnumeric_la_SOURCES = $(NUMERIC_SRC)
numeric_libnumeric_la_CPPFLAGS = \
  $(liboctave_la_CPPFLAGS) \
  -I$(srcdir)/cruft/Faddeeva \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_la_LIBADD += numeric/libnumeric.la

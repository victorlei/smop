EXTRA_DIST += \
  octave-value/module.mk

OV_INTTYPE_INC = \
  octave-value/ov-base-int.h \
  octave-value/ov-base-int.cc \
  octave-value/ov-int-traits.h \
  octave-value/ov-int16.h \
  octave-value/ov-int32.h \
  octave-value/ov-int64.h \
  octave-value/ov-int8.h \
  octave-value/ov-intx.h \
  octave-value/ov-uint16.h \
  octave-value/ov-uint32.h \
  octave-value/ov-uint64.h \
  octave-value/ov-uint8.h

OV_SPARSE_INC = \
  octave-value/ov-base-sparse.h \
  octave-value/ov-bool-sparse.h \
  octave-value/ov-cx-sparse.h \
  octave-value/ov-re-sparse.h

OCTAVE_VALUE_INC = \
  octave-value/ov-base-diag.h \
  octave-value/ov-base-diag.cc \
  octave-value/ov-base-mat.h \
  octave-value/ov-base-mat.cc \
  octave-value/ov-base-scalar.h \
  octave-value/ov-base-scalar.cc \
  octave-value/ov-base.h \
  octave-value/ov-bool-mat.h \
  octave-value/ov-bool-mat.cc \
  octave-value/ov-bool.h \
  octave-value/ov-builtin.h \
  octave-value/ov-cell.h \
  octave-value/ov-ch-mat.h \
  octave-value/ov-class.h \
  octave-value/ov-classdef.h \
  octave-value/ov-colon.h \
  octave-value/ov-complex.h \
  octave-value/ov-cs-list.h \
  octave-value/ov-cx-diag.h \
  octave-value/ov-cx-mat.h \
  octave-value/ov-dld-fcn.h \
  octave-value/ov-fcn-handle.h \
  octave-value/ov-fcn-inline.h \
  octave-value/ov-fcn.h \
  octave-value/ov-float.h \
  octave-value/ov-flt-complex.h \
  octave-value/ov-flt-cx-diag.h \
  octave-value/ov-flt-cx-mat.h \
  octave-value/ov-flt-re-diag.h \
  octave-value/ov-flt-re-mat.h \
  octave-value/ov-java.h \
  octave-value/ov-lazy-idx.h \
  octave-value/ov-mex-fcn.h \
  octave-value/ov-null-mat.h \
  octave-value/ov-oncleanup.h \
  octave-value/ov-perm.h \
  octave-value/ov-range.h \
  octave-value/ov-re-diag.h \
  octave-value/ov-re-mat.h \
  octave-value/ov-scalar.h \
  octave-value/ov-str-mat.h \
  octave-value/ov-struct.h \
  octave-value/ov-type-conv.h \
  octave-value/ov-typeinfo.h \
  octave-value/ov-usr-fcn.h \
  octave-value/ov.h \
  $(OV_INTTYPE_INC) \
  $(OV_SPARSE_INC)

OV_INTTYPE_SRC = \
  octave-value/ov-int16.cc \
  octave-value/ov-int32.cc \
  octave-value/ov-int64.cc \
  octave-value/ov-int8.cc \
  octave-value/ov-uint16.cc \
  octave-value/ov-uint32.cc \
  octave-value/ov-uint64.cc \
  octave-value/ov-uint8.cc

OV_SPARSE_SRC = \
  octave-value/ov-base-sparse.cc \
  octave-value/ov-bool-sparse.cc \
  octave-value/ov-cx-sparse.cc \
  octave-value/ov-re-sparse.cc

OCTAVE_VALUE_SRC = \
  octave-value/ov-base.cc \
  octave-value/ov-bool-mat.cc \
  octave-value/ov-bool.cc \
  octave-value/ov-builtin.cc \
  octave-value/ov-cell.cc \
  octave-value/ov-ch-mat.cc \
  octave-value/ov-class.cc \
  octave-value/ov-classdef.cc \
  octave-value/ov-colon.cc \
  octave-value/ov-complex.cc \
  octave-value/ov-cs-list.cc \
  octave-value/ov-cx-diag.cc \
  octave-value/ov-cx-mat.cc \
  octave-value/ov-dld-fcn.cc \
  octave-value/ov-fcn-handle.cc \
  octave-value/ov-fcn-inline.cc \
  octave-value/ov-fcn.cc \
  octave-value/ov-float.cc \
  octave-value/ov-flt-complex.cc \
  octave-value/ov-flt-cx-diag.cc \
  octave-value/ov-flt-cx-mat.cc \
  octave-value/ov-flt-re-diag.cc \
  octave-value/ov-flt-re-mat.cc \
  octave-value/ov-java.cc \
  octave-value/ov-lazy-idx.cc \
  octave-value/ov-mex-fcn.cc \
  octave-value/ov-null-mat.cc \
  octave-value/ov-oncleanup.cc \
  octave-value/ov-perm.cc \
  octave-value/ov-range.cc \
  octave-value/ov-re-diag.cc \
  octave-value/ov-re-mat.cc \
  octave-value/ov-scalar.cc \
  octave-value/ov-str-mat.cc \
  octave-value/ov-struct.cc \
  octave-value/ov-typeinfo.cc \
  octave-value/ov-usr-fcn.cc \
  octave-value/ov.cc \
  $(OV_INTTYPE_SRC) \
  $(OV_SPARSE_SRC)

OV_JAVA_DF = \
  octave-value/ov.df \
  octave-value/ov-class.df \
  octave-value/ov-java.df \
  octave-value/ov-typeinfo.df

## Special rules for Java .df files so that not all .df files are built with
## JAVA_CPPFLAGS
$(OV_JAVA_DF) : octave-value/%.df : octave-value/%.cc $(GENERATED_MAKE_BUILTINS_INCS)
	$(AM_V_GEN)rm -f $@-t $@-t1 $@ && \
	$(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	  $(AM_CPPFLAGS) $(JAVA_CPPFLAGS) $(CPPFLAGS) \
	  $(AM_CXXFLAGS) $(CXXFLAGS) \
	  -DMAKE_BUILTINS $< > $@-t1 && \
	$(srcdir)/mkdefs $(srcdir) $< < $@-t1 > $@-t && \
	mv $@-t $@ && \
	rm -f $@-t1

noinst_LTLIBRARIES += octave-value/liboctave-value.la

octave_value_liboctave_value_la_SOURCES = $(OCTAVE_VALUE_SRC)

## FIXME -- maybe it would be better to limit the JAVA flags to
## the compile commands for ov-java.cc?  Does JAVA_LIBS need to be
## added to LIBOCTINTERP_LINK_DEPS (see libinterp/link-deps.mk)?
## Should we have a separate set of JAVA_LDFLAGS?

octave_value_liboctave_value_la_CPPFLAGS = \
  $(liboctinterp_la_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(JAVA_CPPFLAGS)

octave_value_liboctave_value_la_LIBADD = $(JAVA_LIBS)

EXTRA_DIST += \
  operators/module.mk \
  operators/config-ops.sh \
  operators/mk-ops.awk \
  operators/mx-op-inc.mk \
  operators/mx-op-src.mk \
  operators/mx-ops \
  operators/smx-op-inc.mk \
  operators/smx-op-src.mk \
  operators/sparse-mk-ops.awk \
  operators/smx-ops \
  operators/vx-op-inc.mk \
  operators/vx-op-src.mk \
  operators/vx-ops

include operators/vx-op-inc.mk
include operators/mx-op-inc.mk
include operators/smx-op-inc.mk

include operators/vx-op-src.mk
include operators/mx-op-src.mk
include operators/smx-op-src.mk

BUILT_LIBOPERATORS_SOURCES = \
  $(MX_OP_SRC) \
  $(VX_OP_SRC) \
  $(SMX_OP_SRC)

OPERATORS_INC = \
  operators/mx-base.h \
  operators/mx-defs.h \
  operators/mx-ext.h \
  operators/mx-op-decl.h \
  operators/mx-op-defs.h \
  operators/Sparse-diag-op-defs.h \
  operators/Sparse-op-decls.h \
  operators/Sparse-op-defs.h \
  operators/Sparse-perm-op-defs.h

## There are no distributed source files in this directory
OPERATORS_SRC =

TEMPLATE_SRC += \
  operators/mx-inlines.cc

OP_SRCDIR = $(abs_top_srcdir)/liboctave/operators

define run-mx-ops
  ( cd operators; \
    $(AWK) -f $(OP_SRCDIR)/$(2)mk-ops.awk prefix=$(1) $(OP_SRCDIR)/$(1)-ops \
  )
endef

## Special rules for sources which must be built before rest of compilation.
$(VX_OP_INC) $(VX_OP_SRC) : operators/mk-ops.awk operators/vx-ops
	$(AM_V_GEN)$(call run-mx-ops,vx)

$(MX_OP_INC) $(MX_OP_SRC) : operators/mk-ops.awk operators/mx-ops
	$(AM_V_GEN)$(call run-mx-ops,mx)

$(SMX_OP_INC) $(SMX_OP_SRC) : operators/sparse-mk-ops.awk operators/smx-ops
	$(AM_V_GEN)$(call run-mx-ops,smx,sparse-)

operators/mx-ops.h : operators/mk-ops.awk operators/mx-ops
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(OP_SRCDIR)/mk-ops.awk prefix=mx make_inclusive_header=mx-ops.h $(OP_SRCDIR)/mx-ops > $@-t && \
	mv $@-t $@

noinst_LTLIBRARIES += operators/liboperators.la

operators_liboperators_la_SOURCES = $(OPERATORS_SRC)
nodist_operators_liboperators_la_SOURCES = $(BUILT_LIBOPERATORS_SOURCES)

operators_liboperators_la_CPPFLAGS = $(liboctave_la_CPPFLAGS)

DISTCLEANFILES += $(BUILT_LIBOPERATORS_SOURCES)

liboctave_la_LIBADD += operators/liboperators.la

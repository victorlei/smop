EXTRA_DIST += \
  corefcn/module.mk \
  corefcn/defaults.in.h \
  corefcn/graphics.in.h \
  corefcn/mxarray.in.h \
  corefcn/oct-errno.in.cc \
  corefcn/oct-tex-lexer.in.ll \
  corefcn/oct-tex-parser.in.yy \
  corefcn/oct-tex-symbols.in

## Options functions for Fortran packages like LSODE, DASPK.
## These are generated automagically by configure and Perl.
OPT_HANDLERS = \
  corefcn/DASPK-opts.cc \
  corefcn/DASRT-opts.cc \
  corefcn/DASSL-opts.cc \
  corefcn/LSODE-opts.cc \
  corefcn/Quad-opts.cc

OPT_INC = \
  $(top_builddir)/liboctave/numeric/DASPK-opts.h \
  $(top_builddir)/liboctave/numeric/DASRT-opts.h \
  $(top_builddir)/liboctave/numeric/DASSL-opts.h \
  $(top_builddir)/liboctave/numeric/LSODE-opts.h \
  $(top_builddir)/liboctave/numeric/Quad-opts.h

$(OPT_HANDLERS): corefcn/%.cc : $(top_builddir)/liboctave/numeric/%.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(top_srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t && \
	mv $@-t $@

$(OPT_HANDLERS): $(top_srcdir)/build-aux/mk-opts.pl

$(OPT_INC) : %.h : %.in
	$(AM_V_GEN)$(MAKE) -C $(top_builddir)/liboctave/numeric $(@F)

JIT_INC = \
  corefcn/jit-util.h \
  corefcn/jit-typeinfo.h \
  corefcn/jit-ir.h \
  corefcn/pt-jit.h

TEX_PARSER_INC = \
  corefcn/oct-tex-parser.h

COREFCN_INC = \
  corefcn/Cell.h \
  corefcn/c-file-ptr-stream.h \
  corefcn/cdisplay.h \
  corefcn/comment-list.h \
  corefcn/cutils.h \
  corefcn/data.h \
  corefcn/debug.h \
  corefcn/defun-dld.h \
  corefcn/defun-int.h \
  corefcn/defun.h \
  corefcn/dirfns.h \
  corefcn/display.h \
  corefcn/dynamic-ld.h \
  corefcn/error.h \
  corefcn/event-queue.h \
  corefcn/file-io.h \
  corefcn/gl-render.h \
  corefcn/gl2ps-renderer.h \
  corefcn/gripes.h \
  corefcn/help.h \
  corefcn/hook-fcn.h \
  corefcn/input.h \
  corefcn/load-path.h \
  corefcn/load-save.h \
  corefcn/ls-ascii-helper.h \
  corefcn/ls-hdf5.h \
  corefcn/ls-mat-ascii.h \
  corefcn/ls-mat4.h \
  corefcn/ls-mat5.h \
  corefcn/ls-oct-ascii.h \
  corefcn/ls-oct-binary.h \
  corefcn/ls-utils.h \
  corefcn/mex.h \
  corefcn/mexproto.h \
  corefcn/oct-errno.h \
  corefcn/oct-fstrm.h \
  corefcn/oct-handle.h \
  corefcn/oct-hdf5.h \
  corefcn/oct-hdf5-id.h \
  corefcn/oct-hist.h \
  corefcn/oct-iostrm.h \
  corefcn/oct-lvalue.h \
  corefcn/oct-map.h \
  corefcn/oct-obj.h \
  corefcn/oct-prcstrm.h \
  corefcn/oct-procbuf.h \
  corefcn/oct-stdstrm.h \
  corefcn/oct-stream.h \
  corefcn/oct-strstrm.h \
  corefcn/oct.h \
  corefcn/octave-default-image.h \
  corefcn/octave-link.h \
  corefcn/pager.h \
  corefcn/pr-output.h \
  corefcn/procstream.h \
  corefcn/profiler.h \
  corefcn/sighandlers.h \
  corefcn/siglist.h \
  corefcn/sparse-xdiv.h \
  corefcn/sparse-xpow.h \
  corefcn/symtab.h \
  corefcn/sysdep.h \
  corefcn/toplev.h \
  corefcn/txt-eng-ft.h \
  corefcn/txt-eng.h \
  corefcn/utils.h \
  corefcn/variables.h \
  corefcn/workspace-element.h \
  corefcn/xdiv.h \
  corefcn/xnorm.h \
  corefcn/xpow.h \
  corefcn/zfstream.h \
  $(JIT_INC) \
  $(TEX_PARSER_INC)

JIT_SRC = \
  corefcn/jit-util.cc \
  corefcn/jit-typeinfo.cc \
  corefcn/jit-ir.cc \
  corefcn/pt-jit.cc

TEX_PARSER_SRC = \
  corefcn/oct-tex-lexer.ll \
  corefcn/oct-tex-parser.yy

C_COREFCN_SRC = \
  corefcn/cutils.c \
  corefcn/matherr.c \
  corefcn/siglist.c

COREFCN_SRC = \
  corefcn/Cell.cc \
  corefcn/__contourc__.cc \
  corefcn/__dispatch__.cc \
  corefcn/__dsearchn__.cc \
  corefcn/__ichol__.cc \
  corefcn/__ilu__.cc \
  corefcn/__lin_interpn__.cc \
  corefcn/__pchip_deriv__.cc \
  corefcn/__qp__.cc \
  corefcn/balance.cc \
  corefcn/besselj.cc \
  corefcn/betainc.cc \
  corefcn/bitfcns.cc \
  corefcn/bsxfun.cc \
  corefcn/c-file-ptr-stream.cc \
  corefcn/cdisplay.c \
  corefcn/cellfun.cc \
  corefcn/colloc.cc \
  corefcn/comment-list.cc \
  corefcn/conv2.cc \
  corefcn/daspk.cc \
  corefcn/dasrt.cc \
  corefcn/dassl.cc \
  corefcn/data.cc \
  corefcn/debug.cc \
  corefcn/defaults.cc \
  corefcn/defun.cc \
  corefcn/det.cc \
  corefcn/dirfns.cc \
  corefcn/display.cc \
  corefcn/dlmread.cc \
  corefcn/dot.cc \
  corefcn/dynamic-ld.cc \
  corefcn/eig.cc \
  corefcn/ellipj.cc \
  corefcn/error.cc \
  corefcn/fft.cc \
  corefcn/fft2.cc \
  corefcn/fftn.cc \
  corefcn/file-io.cc \
  corefcn/filter.cc \
  corefcn/find.cc \
  corefcn/gammainc.cc \
  corefcn/gcd.cc \
  corefcn/getgrent.cc \
  corefcn/getpwent.cc \
  corefcn/getrusage.cc \
  corefcn/givens.cc \
  corefcn/gl-render.cc \
  corefcn/gl2ps-renderer.cc \
  corefcn/graphics.cc \
  corefcn/gripes.cc \
  corefcn/help.cc \
  corefcn/hess.cc \
  corefcn/hex2num.cc \
  corefcn/hook-fcn.cc \
  corefcn/input.cc \
  corefcn/inv.cc \
  corefcn/kron.cc \
  corefcn/load-path.cc \
  corefcn/load-save.cc \
  corefcn/lookup.cc \
  corefcn/ls-ascii-helper.cc \
  corefcn/ls-hdf5.cc \
  corefcn/ls-mat-ascii.cc \
  corefcn/ls-mat4.cc \
  corefcn/ls-mat5.cc \
  corefcn/ls-oct-ascii.cc \
  corefcn/ls-oct-binary.cc \
  corefcn/ls-utils.cc \
  corefcn/lsode.cc \
  corefcn/lu.cc \
  corefcn/luinc.cc \
  corefcn/mappers.cc \
  corefcn/matrix_type.cc \
  corefcn/max.cc \
  corefcn/md5sum.cc \
  corefcn/mex.cc \
  corefcn/mgorth.cc \
  corefcn/nproc.cc \
  corefcn/oct-fstrm.cc \
  corefcn/oct-hdf5-id.cc \
  corefcn/oct-hist.cc \
  corefcn/oct-iostrm.cc \
  corefcn/oct-lvalue.cc \
  corefcn/oct-map.cc \
  corefcn/oct-obj.cc \
  corefcn/oct-prcstrm.cc \
  corefcn/oct-procbuf.cc \
  corefcn/oct-stream.cc \
  corefcn/oct-strstrm.cc \
  corefcn/octave-link.cc \
  corefcn/ordschur.cc \
  corefcn/pager.cc \
  corefcn/pinv.cc \
  corefcn/pr-output.cc \
  corefcn/procstream.cc \
  corefcn/profiler.cc \
  corefcn/quad.cc \
  corefcn/quadcc.cc \
  corefcn/qz.cc \
  corefcn/rand.cc \
  corefcn/rcond.cc \
  corefcn/regexp.cc \
  corefcn/schur.cc \
  corefcn/sighandlers.cc \
  corefcn/sparse-xdiv.cc \
  corefcn/sparse-xpow.cc \
  corefcn/sparse.cc \
  corefcn/spparms.cc \
  corefcn/sqrtm.cc \
  corefcn/str2double.cc \
  corefcn/strfind.cc \
  corefcn/strfns.cc \
  corefcn/sub2ind.cc \
  corefcn/svd.cc \
  corefcn/sylvester.cc \
  corefcn/symtab.cc \
  corefcn/syscalls.cc \
  corefcn/sysdep.cc \
  corefcn/time.cc \
  corefcn/toplev.cc \
  corefcn/tril.cc \
  corefcn/tsearch.cc \
  corefcn/txt-eng-ft.cc \
  corefcn/txt-eng.cc \
  corefcn/typecast.cc \
  corefcn/urlwrite.cc \
  corefcn/utils.cc \
  corefcn/variables.cc \
  corefcn/xdiv.cc \
  corefcn/xnorm.cc \
  corefcn/xpow.cc \
  corefcn/zfstream.cc \
  $(JIT_SRC) \
  $(C_COREFCN_SRC)

COREFCN_FT2_DF = \
  corefcn/graphics.df \
  corefcn/gl-render.df \
  corefcn/toplev.df \
  corefcn/txt-eng-ft.df

## FIXME: Automake does not support per-object rules.
##        These rules could be emulated by creating a new convenience
##        library and using per-library rules.  Or we can just live
##        without the rule since there haven't been any problems. (09/18/2012)
#display.df display.lo: CPPFLAGS += $(X11_FLAGS)

## Special rules for FreeType .df files so that not all .df files are built
## with FT2_CPPFLAGS, FONTCONFIG_CPPFLAGS
$(COREFCN_FT2_DF) : corefcn/%.df : corefcn/%.cc $(GENERATED_MAKE_BUILTINS_INCS)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	  $(AM_CPPFLAGS) $(FONTCONFIG_CPPFLAGS) $(FT2_CPPFLAGS) $(CPPFLAGS) \
	  $(AM_CXXFLAGS) $(CXXFLAGS) \
	  -DMAKE_BUILTINS $< > $@-t && \
	$(srcdir)/mkdefs $(srcdir) $< < $@-t > $@ && \
	rm $@-t

## Special rules for sources which must be built before rest of compilation.

## defaults.h and graphics.h must depend on Makefile.  Calling configure
## may change default/config values.  However, calling configure will also
## regenerate the Makefiles from Makefile.am and trigger the rules below.
corefcn/defaults.h: corefcn/defaults.in.h Makefile
	$(AM_V_GEN)$(do_subst_default_vals)

corefcn/graphics.h: corefcn/graphics.in.h genprops.awk Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/genprops.awk $< > $@-t && \
	mv $@-t $@

corefcn/graphics-props.cc: corefcn/graphics.in.h genprops.awk Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/genprops.awk $< > $@-t && \
	mv $@-t $@

corefcn/oct-errno.cc: corefcn/oct-errno.in.cc Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	if test -n "$(PERL)"; then \
	  $(srcdir)/mk-errno-list --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(srcdir)/mk-errno-list --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SED) '/@SYSDEP_ERRNO_LIST@/D' $< > $@-t; \
	fi && \
	mv $@-t $@

corefcn/mxarray.h: corefcn/mxarray.in.h Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< \
	  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
	  -e "s|%OCTAVE_IDX_TYPE%|${OCTAVE_IDX_TYPE}|" > $@-t && \
	mv $@-t $@

corefcn/oct-tex-lexer.ll: corefcn/oct-tex-lexer.in.ll corefcn/oct-tex-symbols.in Makefile.am
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) 'BEGIN { print "/* DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-lexer.in.ll and oct-tex-symbols.in. */"; } /^@SYMBOL_RULES@$$/ { count = 0; while (getline < "$(srcdir)/corefcn/oct-tex-symbols.in") { if ($$0 !~ /^#.*/ && NF == 3) { printf("\"\\\\%s\" { yylval->sym = %d; return SYM; }\n", $$1, count); count++; } } getline } ! /^@SYMBOL_RULES@$$/ { print }' $< > $@-t && \
	mv $@-t $@

corefcn/oct-tex-symbols.cc: corefcn/oct-tex-symbols.in Makefile.am
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) 'BEGIN { print "// DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-symbols.in."; print "static uint32_t symbol_codes[][2] = {"; count = 0; } END { print "};"; printf("static int num_symbol_codes = %d;\n", count); } !/^#/ && (NF == 3) { printf("  { %s, %s },\n", $$2, $$3); count++; }' $< > $@-t && \
	mv $@-t $@

corefcn/txt-eng.cc: corefcn/oct-tex-symbols.cc
corefcn/oct-tex-lexer.cc: LEX_OUTPUT_ROOT := lex.octave_tex_
corefcn/oct-tex-parser.h: corefcn/oct-tex-parser.yy

corefcn/oct-tex-parser.yy: corefcn/oct-tex-parser.in.yy
	$(AM_V_GEN)$(call subst-bison-api-decls,octave_tex_)

noinst_LTLIBRARIES += \
  corefcn/libcorefcn.la \
  corefcn/libtex_parser.la

corefcn_libcorefcn_la_SOURCES = $(COREFCN_SRC)
corefcn_libcorefcn_la_CPPFLAGS = $(liboctinterp_la_CPPFLAGS) \
                                 $(FFTW_XCPPFLAGS) \
                                 $(FONTCONFIG_CPPFLAGS) \
                                 $(FT2_CPPFLAGS) \
                                 $(HDF5_CPPFLAGS) \
                                 $(LLVM_CPPFLAGS) \
                                 $(Z_CPPFLAGS)

corefcn_libcorefcn_la_CXXFLAGS = $(AM_CXXFLAGS) $(LLVM_CXXFLAGS)

corefcn_libtex_parser_la_SOURCES = $(TEX_PARSER_SRC)
corefcn_libtex_parser_la_CPPFLAGS = $(liboctinterp_la_CPPFLAGS)
corefcn_libtex_parser_la_CXXFLAGS = \
  $(filter-out -Wold-style-cast, $(AM_CXXFLAGS))


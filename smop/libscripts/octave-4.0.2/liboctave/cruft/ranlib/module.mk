EXTRA_DIST += \
  cruft/ranlib/module.mk \
  cruft/ranlib/Basegen.doc \
  cruft/ranlib/HOWTOGET \
  cruft/ranlib/README \
  cruft/ranlib/randlib.chs \
  cruft/ranlib/randlib.fdoc \
  cruft/ranlib/tstbot.for \
  cruft/ranlib/tstgmn.for \
  cruft/ranlib/tstmid.for

RANLIB_SRC = \
  cruft/ranlib/advnst.f \
  cruft/ranlib/genbet.f \
  cruft/ranlib/genchi.f \
  cruft/ranlib/genexp.f \
  cruft/ranlib/genf.f \
  cruft/ranlib/gengam.f \
  cruft/ranlib/genmn.f \
  cruft/ranlib/genmul.f \
  cruft/ranlib/gennch.f \
  cruft/ranlib/gennf.f \
  cruft/ranlib/gennor.f \
  cruft/ranlib/genprm.f \
  cruft/ranlib/genunf.f \
  cruft/ranlib/getcgn.f \
  cruft/ranlib/getsd.f \
  cruft/ranlib/ignbin.f \
  cruft/ranlib/ignlgi.f \
  cruft/ranlib/ignnbn.f \
  cruft/ranlib/ignpoi.f \
  cruft/ranlib/ignuin.f \
  cruft/ranlib/initgn.f \
  cruft/ranlib/inrgcm.f \
  cruft/ranlib/lennob.f \
  cruft/ranlib/mltmod.f \
  cruft/ranlib/phrtsd.f \
  cruft/ranlib/qrgnin.f \
  cruft/ranlib/ranf.f \
  cruft/ranlib/setall.f \
  cruft/ranlib/setant.f \
  cruft/ranlib/setgmn.f \
  cruft/ranlib/setsd.f \
  cruft/ranlib/sexpo.f \
  cruft/ranlib/sgamma.f \
  cruft/ranlib/snorm.f \
  cruft/ranlib/wrap.f

noinst_LTLIBRARIES += cruft/ranlib/libranlib.la

cruft_ranlib_libranlib_la_SOURCES = $(RANLIB_SRC)

cruft_ranlib_libranlib_la_DEPENDENCIES = cruft/ranlib/ranlib.def

define gen-ranlib-def
  rm -f $@-t $@ && \
  $(MKDIR_P) cruft/ranlib && \
  $(SHELL) cruft/mkf77def $(srcdir) $(RANLIB_SRC) > $@-t && \
  mv $@-t $@
endef

## Special rules for files which must be built before compilation
## ranlib directory may not exist in VPATH build; create it if necessary.
cruft/ranlib/ranlib.def: $(RANLIB_SRC) cruft/mkf77def
	$(AM_V_GEN)$(gen-ranlib-def)

liboctave_la_LIBADD += cruft/ranlib/libranlib.la

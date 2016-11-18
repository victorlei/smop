EXTRA_DIST += \
  cruft/slatec-fn/module.mk \
  cruft/slatec-fn/derfc.in.f \
  cruft/slatec-fn/erfc.in.f

CRUFT_SOURCES += \
  cruft/slatec-fn/albeta.f \
  cruft/slatec-fn/alngam.f \
  cruft/slatec-fn/alnrel.f \
  cruft/slatec-fn/algams.f \
  cruft/slatec-fn/acosh.f \
  cruft/slatec-fn/asinh.f \
  cruft/slatec-fn/atanh.f \
  cruft/slatec-fn/betai.f \
  cruft/slatec-fn/csevl.f \
  cruft/slatec-fn/d9gmit.f \
  cruft/slatec-fn/d9lgic.f \
  cruft/slatec-fn/d9lgit.f \
  cruft/slatec-fn/d9lgmc.f \
  cruft/slatec-fn/dacosh.f \
  cruft/slatec-fn/dasinh.f \
  cruft/slatec-fn/datanh.f \
  cruft/slatec-fn/dbetai.f \
  cruft/slatec-fn/dcsevl.f \
  cruft/slatec-fn/derf.f \
  cruft/slatec-fn/dgami.f \
  cruft/slatec-fn/dgamit.f \
  cruft/slatec-fn/dgamlm.f \
  cruft/slatec-fn/dgamma.f \
  cruft/slatec-fn/dgamr.f \
  cruft/slatec-fn/dlbeta.f \
  cruft/slatec-fn/dlgams.f \
  cruft/slatec-fn/dlngam.f \
  cruft/slatec-fn/dlnrel.f \
  cruft/slatec-fn/dpchim.f \
  cruft/slatec-fn/dpchst.f \
  cruft/slatec-fn/erf.f \
  cruft/slatec-fn/gami.f \
  cruft/slatec-fn/gamit.f \
  cruft/slatec-fn/gamlim.f \
  cruft/slatec-fn/gamma.f \
  cruft/slatec-fn/gamr.f \
  cruft/slatec-fn/initds.f \
  cruft/slatec-fn/inits.f \
  cruft/slatec-fn/pchim.f \
  cruft/slatec-fn/pchst.f \
  cruft/slatec-fn/r9lgmc.f \
  cruft/slatec-fn/r9lgit.f \
  cruft/slatec-fn/r9gmit.f \
  cruft/slatec-fn/r9lgic.f \
  cruft/slatec-fn/xdacosh.f \
  cruft/slatec-fn/xdasinh.f \
  cruft/slatec-fn/xdatanh.f \
  cruft/slatec-fn/xdbetai.f \
  cruft/slatec-fn/xderf.f \
  cruft/slatec-fn/xderfc.f \
  cruft/slatec-fn/xdgami.f \
  cruft/slatec-fn/xdgamit.f \
  cruft/slatec-fn/xdgamma.f \
  cruft/slatec-fn/xgmainc.f \
  cruft/slatec-fn/xacosh.f \
  cruft/slatec-fn/xasinh.f \
  cruft/slatec-fn/xatanh.f \
  cruft/slatec-fn/xerf.f \
  cruft/slatec-fn/xerfc.f \
  cruft/slatec-fn/xsgmainc.f \
  cruft/slatec-fn/xgamma.f \
  cruft/slatec-fn/xbetai.f

nodist_cruft_libcruft_la_SOURCES += \
  cruft/slatec-fn/derfc.f \
  cruft/slatec-fn/erfc.f

## slatec-fn directory may not exist in VPATH build; create it if necessary.

define do-subst-isnan-macro
  rm -f $@-t $@ && \
  $(MKDIR_P) cruft/slatec-fn && \
  $(SED) -e "${F77_ISNAN_MACRO}" < $< > $@-t && \
  mv $@-t $@
endef

cruft/slatec-fn/erfc.f: cruft/slatec-fn/erfc.in.f Makefile
	$(AM_V_GEN)$(do-subst-isnan-macro)

cruft/slatec-fn/derfc.f: cruft/slatec-fn/derfc.in.f Makefile
	$(AM_V_GEN)$(do-subst-isnan-macro)


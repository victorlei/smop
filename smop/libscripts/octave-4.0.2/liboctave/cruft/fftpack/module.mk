EXTRA_DIST += \
  cruft/fftpack/module.mk \
  cruft/fftpack/fftpack.doc

FFTPACK_SRC = \
  cruft/fftpack/cfftb.f \
  cruft/fftpack/cfftb1.f \
  cruft/fftpack/cfftf.f \
  cruft/fftpack/cfftf1.f \
  cruft/fftpack/cffti.f \
  cruft/fftpack/cffti1.f \
  cruft/fftpack/passb.f \
  cruft/fftpack/passb2.f \
  cruft/fftpack/passb3.f \
  cruft/fftpack/passb4.f \
  cruft/fftpack/passb5.f \
  cruft/fftpack/passf.f \
  cruft/fftpack/passf2.f \
  cruft/fftpack/passf3.f \
  cruft/fftpack/passf4.f \
  cruft/fftpack/passf5.f \
  cruft/fftpack/zfftb.f \
  cruft/fftpack/zfftb1.f \
  cruft/fftpack/zfftf.f \
  cruft/fftpack/zfftf1.f \
  cruft/fftpack/zffti.f \
  cruft/fftpack/zffti1.f \
  cruft/fftpack/zpassb.f \
  cruft/fftpack/zpassb2.f \
  cruft/fftpack/zpassb3.f \
  cruft/fftpack/zpassb4.f \
  cruft/fftpack/zpassb5.f \
  cruft/fftpack/zpassf.f \
  cruft/fftpack/zpassf2.f \
  cruft/fftpack/zpassf3.f \
  cruft/fftpack/zpassf4.f \
  cruft/fftpack/zpassf5.f

if AMCOND_HAVE_FFTW
  EXTRA_DIST += $(FFTPACK_SRC)
else
  CRUFT_SOURCES += $(FFTPACK_SRC)
endif

FCN_FILE_DIRS += signal

signal_PRIVATE_FCN_FILES = \
  signal/private/rectangle_lw.m  \
  signal/private/rectangle_sw.m  \
  signal/private/triangle_lw.m  \
  signal/private/triangle_sw.m

signal_FCN_FILES = \
  signal/arch_fit.m \
  signal/arch_rnd.m \
  signal/arch_test.m \
  signal/arma_rnd.m \
  signal/autoreg_matrix.m \
  signal/bartlett.m \
  signal/blackman.m \
  signal/detrend.m \
  signal/diffpara.m \
  signal/durbinlevinson.m \
  signal/fftconv.m \
  signal/fftfilt.m \
  signal/fftshift.m \
  signal/filter2.m \
  signal/fractdiff.m \
  signal/freqz.m \
  signal/freqz_plot.m \
  signal/hamming.m \
  signal/hanning.m \
  signal/hurst.m \
  signal/ifftshift.m \
  signal/periodogram.m \
  signal/sinc.m \
  signal/sinetone.m \
  signal/sinewave.m \
  signal/spectral_adf.m \
  signal/spectral_xdf.m \
  signal/spencer.m \
  signal/stft.m \
  signal/synthesis.m \
  signal/unwrap.m \
  signal/yulewalker.m \
  $(signal_PRIVATE_FCN_FILES)

FCN_FILES += $(signal_FCN_FILES)

PKG_ADD_FILES += signal/PKG_ADD

DIRSTAMP_FILES += signal/$(octave_dirstamp)

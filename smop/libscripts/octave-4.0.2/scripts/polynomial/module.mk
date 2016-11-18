FCN_FILE_DIRS += polynomial

polynomial_PRIVATE_FCN_FILES = \
  polynomial/private/__splinefit__.m

polynomial_FCN_FILES = \
  polynomial/compan.m \
  polynomial/conv.m \
  polynomial/deconv.m \
  polynomial/mkpp.m \
  polynomial/mpoles.m \
  polynomial/pchip.m \
  polynomial/poly.m \
  polynomial/polyaffine.m \
  polynomial/polyder.m \
  polynomial/polyeig.m \
  polynomial/polyfit.m \
  polynomial/polygcd.m \
  polynomial/polyint.m \
  polynomial/polyout.m \
  polynomial/polyreduce.m \
  polynomial/polyval.m \
  polynomial/polyvalm.m \
  polynomial/ppval.m \
  polynomial/ppder.m \
  polynomial/ppint.m \
  polynomial/ppjumps.m \
  polynomial/residue.m \
  polynomial/roots.m \
  polynomial/spline.m \
  polynomial/splinefit.m \
  polynomial/unmkpp.m \
  $(polynomial_PRIVATE_FCN_FILES)

FCN_FILES += $(polynomial_FCN_FILES)

PKG_ADD_FILES += polynomial/PKG_ADD

DIRSTAMP_FILES += polynomial/$(octave_dirstamp)

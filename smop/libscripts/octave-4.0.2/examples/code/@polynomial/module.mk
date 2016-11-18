FCN_FILE_DIRS += @polynomial

EXTRA_DIST += @polynomial/module.mk

at_polynomial_FCN_FILES = \
  @polynomial/display.m \
  @polynomial/double.m \
  @polynomial/end.m \
  @polynomial/get.m \
  @polynomial/mtimes.m \
  @polynomial/numel.m \
  @polynomial/plot.m \
  @polynomial/polynomial.m \
  @polynomial/polynomial_superiorto.m \
  @polynomial/polyval.m \
  @polynomial/roots.m \
  @polynomial/set.m \
  @polynomial/subsasgn.m \
  @polynomial/subsref.m

FCN_FILES += $(at_polynomial_FCN_FILES)

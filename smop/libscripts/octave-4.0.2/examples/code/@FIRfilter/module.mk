FCN_FILE_DIRS += @FIRfilter

EXTRA_DIST += @FIRfilter/module.mk

at_FIRfilter_FCN_FILES = \
  @FIRfilter/display.m \
  @FIRfilter/FIRfilter_aggregation.m \
  @FIRfilter/FIRfilter.m \
  @FIRfilter/subsasgn.m \
  @FIRfilter/subsref.m

FCN_FILES += $(at_FIRfilter_FCN_FILES)

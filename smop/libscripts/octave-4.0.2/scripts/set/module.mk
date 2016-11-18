FCN_FILE_DIRS += set

set_FCN_FILES = \
  set/intersect.m \
  set/ismember.m \
  set/powerset.m \
  set/setdiff.m \
  set/setxor.m \
  set/union.m \
  set/unique.m \
  set/private/validsetargs.m

FCN_FILES += $(set_FCN_FILES)

PKG_ADD_FILES += set/PKG_ADD

DIRSTAMP_FILES += set/$(octave_dirstamp)

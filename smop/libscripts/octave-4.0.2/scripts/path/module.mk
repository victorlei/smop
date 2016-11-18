FCN_FILE_DIRS += path

path_PRIVATE_FCN_FILES = \
  path/private/getsavepath.m

path_FCN_FILES = \
  path/matlabroot.m \
  path/pathdef.m \
  path/savepath.m \
  $(path_PRIVATE_FCN_FILES)

FCN_FILES += $(path_FCN_FILES)

PKG_ADD_FILES += path/PKG_ADD

DIRSTAMP_FILES += path/$(octave_dirstamp)

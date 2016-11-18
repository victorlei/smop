FCN_FILE_DIRS += prefs

prefs_PRIVATE_FCN_FILES = \
  prefs/private/loadprefs.m \
  prefs/private/prefsfile.m \
  prefs/private/saveprefs.m

prefs_FCN_FILES = \
  prefs/addpref.m \
  prefs/getpref.m \
  prefs/ispref.m \
  prefs/prefdir.m \
  prefs/preferences.m \
  prefs/rmpref.m \
  prefs/setpref.m \
  $(prefs_PRIVATE_FCN_FILES)

FCN_FILES += $(prefs_FCN_FILES)

PKG_ADD_FILES += prefs/PKG_ADD

DIRSTAMP_FILES += prefs/$(octave_dirstamp)

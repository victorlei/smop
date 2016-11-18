FCN_FILE_DIRS += elfun

elfun_FCN_FILES = \
  elfun/acosd.m \
  elfun/acot.m \
  elfun/acotd.m \
  elfun/acoth.m \
  elfun/acsc.m \
  elfun/acscd.m \
  elfun/acsch.m \
  elfun/asec.m \
  elfun/asecd.m \
  elfun/asech.m \
  elfun/asind.m \
  elfun/atan2d.m \
  elfun/atand.m \
  elfun/cosd.m \
  elfun/cot.m \
  elfun/cotd.m \
  elfun/coth.m \
  elfun/csc.m \
  elfun/cscd.m \
  elfun/csch.m \
  elfun/sec.m \
  elfun/secd.m \
  elfun/sech.m \
  elfun/sind.m \
  elfun/tand.m

FCN_FILES += $(elfun_FCN_FILES)

PKG_ADD_FILES += elfun/PKG_ADD

DIRSTAMP_FILES += elfun/$(octave_dirstamp)

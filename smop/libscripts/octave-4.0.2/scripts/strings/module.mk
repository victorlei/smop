FCN_FILE_DIRS += strings

strings_FCN_FILES = \
  strings/base2dec.m \
  strings/bin2dec.m \
  strings/blanks.m \
  strings/cstrcat.m \
  strings/deblank.m \
  strings/dec2base.m \
  strings/dec2bin.m \
  strings/dec2hex.m \
  strings/findstr.m \
  strings/hex2dec.m \
  strings/index.m \
  strings/isletter.m \
  strings/isstrprop.m \
  strings/mat2str.m \
  strings/ostrsplit.m \
  strings/regexptranslate.m \
  strings/rindex.m \
  strings/str2num.m \
  strings/strcat.m \
  strings/strchr.m \
  strings/strjoin.m \
  strings/strjust.m \
  strings/strmatch.m \
  strings/strsplit.m \
  strings/strtok.m \
  strings/strtrim.m \
  strings/strtrunc.m \
  strings/substr.m \
  strings/untabify.m \
  strings/validatestring.m

FCN_FILES += $(strings_FCN_FILES)

PKG_ADD_FILES += strings/PKG_ADD

DIRSTAMP_FILES += strings/$(octave_dirstamp)

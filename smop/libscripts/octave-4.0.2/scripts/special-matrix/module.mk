FCN_FILE_DIRS += special-matrix

special_matrix_FCN_FILES = \
  special-matrix/gallery.m \
  special-matrix/hadamard.m \
  special-matrix/hankel.m \
  special-matrix/hilb.m \
  special-matrix/invhilb.m \
  special-matrix/magic.m \
  special-matrix/pascal.m \
  special-matrix/rosser.m \
  special-matrix/toeplitz.m \
  special-matrix/vander.m \
  special-matrix/wilkinson.m

FCN_FILES += $(special_matrix_FCN_FILES)

PKG_ADD_FILES += special-matrix/PKG_ADD

DIRSTAMP_FILES += special-matrix/$(octave_dirstamp)

FCN_FILE_DIRS += linear-algebra

linear_algebra_FCN_FILES = \
  linear-algebra/bandwidth.m \
  linear-algebra/commutation_matrix.m \
  linear-algebra/cond.m \
  linear-algebra/condest.m \
  linear-algebra/cross.m \
  linear-algebra/duplication_matrix.m \
  linear-algebra/expm.m \
  linear-algebra/housh.m \
  linear-algebra/isbanded.m \
  linear-algebra/isdefinite.m \
  linear-algebra/isdiag.m \
  linear-algebra/ishermitian.m \
  linear-algebra/issymmetric.m \
  linear-algebra/istril.m \
  linear-algebra/istriu.m \
  linear-algebra/krylov.m \
  linear-algebra/linsolve.m \
  linear-algebra/logm.m \
  linear-algebra/normest.m \
  linear-algebra/null.m \
  linear-algebra/onenormest.m \
  linear-algebra/orth.m \
  linear-algebra/planerot.m \
  linear-algebra/qzhess.m \
  linear-algebra/rank.m \
  linear-algebra/rref.m \
  linear-algebra/subspace.m \
  linear-algebra/trace.m \
  linear-algebra/vech.m

FCN_FILES += $(linear_algebra_FCN_FILES)

PKG_ADD_FILES += linear-algebra/PKG_ADD

DIRSTAMP_FILES += linear-algebra/$(octave_dirstamp)

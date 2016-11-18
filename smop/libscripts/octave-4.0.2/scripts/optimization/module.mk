FCN_FILE_DIRS += optimization

optimization_PRIVATE_FCN_FILES = \
  optimization/private/__fdjac__.m

optimization_FCN_FILES = \
  optimization/__all_opts__.m \
  optimization/fminbnd.m \
  optimization/fminsearch.m \
  optimization/fminunc.m \
  optimization/fsolve.m \
  optimization/fzero.m \
  optimization/glpk.m \
  optimization/lsqnonneg.m \
  optimization/optimget.m \
  optimization/optimset.m \
  optimization/pqpnonneg.m \
  optimization/qp.m \
  optimization/sqp.m \
  $(optimization_PRIVATE_FCN_FILES)

FCN_FILES += $(optimization_FCN_FILES)

PKG_ADD_FILES += optimization/PKG_ADD

DIRSTAMP_FILES += optimization/$(octave_dirstamp)

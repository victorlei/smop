FCN_FILE_DIRS += sparse

sparse_PRIVATE_FCN_FILES = \
  sparse/private/__sprand_impl__.m

sparse_FCN_FILES = \
  sparse/bicg.m \
  sparse/bicgstab.m \
  sparse/cgs.m \
  sparse/colperm.m \
  sparse/eigs.m \
  sparse/etreeplot.m \
  sparse/gmres.m \
  sparse/gplot.m \
  sparse/ichol.m \
  sparse/ilu.m \
  sparse/nonzeros.m \
  sparse/pcg.m \
  sparse/pcr.m \
  sparse/qmr.m \
  sparse/spaugment.m \
  sparse/spconvert.m \
  sparse/spdiags.m \
  sparse/speye.m \
  sparse/spfun.m \
  sparse/spones.m \
  sparse/sprand.m \
  sparse/sprandn.m \
  sparse/sprandsym.m \
  sparse/spstats.m \
  sparse/spy.m \
  sparse/svds.m \
  sparse/treelayout.m \
  sparse/treeplot.m \
  $(sparse_PRIVATE_FCN_FILES)

FCN_FILES += $(sparse_FCN_FILES)

PKG_ADD_FILES += sparse/PKG_ADD

DIRSTAMP_FILES += sparse/$(octave_dirstamp)

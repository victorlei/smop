FCN_FILE_DIRS += geometry

geometry_FCN_FILES = \
  geometry/convhull.m \
  geometry/delaunayn.m \
  geometry/delaunay.m \
  geometry/dsearch.m \
  geometry/dsearchn.m \
  geometry/griddata.m \
  geometry/griddata3.m \
  geometry/griddatan.m \
  geometry/inpolygon.m \
  geometry/rectint.m \
  geometry/tsearchn.m \
  geometry/voronoi.m \
  geometry/voronoin.m

FCN_FILES += $(geometry_FCN_FILES)

PKG_ADD_FILES += geometry/PKG_ADD

DIRSTAMP_FILES += geometry/$(octave_dirstamp)

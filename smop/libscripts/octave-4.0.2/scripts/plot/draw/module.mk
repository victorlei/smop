FCN_FILE_DIRS += plot/draw

plot_draw_PRIVATE_FCN_FILES = \
  plot/draw/private/__add_datasource__.m \
  plot/draw/private/__bar__.m \
  plot/draw/private/__contour__.m \
  plot/draw/private/__errcomm__.m \
  plot/draw/private/__errplot__.m \
  plot/draw/private/__ezplot__.m \
  plot/draw/private/__interp_cube__.m \
  plot/draw/private/__line__.m \
  plot/draw/private/__marching_cube__.m \
  plot/draw/private/__patch__.m \
  plot/draw/private/__pie__.m \
  plot/draw/private/__plt__.m \
  plot/draw/private/__quiver__.m \
  plot/draw/private/__scatter__.m \
  plot/draw/private/__stem__.m

plot_draw_FCN_FILES = \
  plot/draw/area.m \
  plot/draw/barh.m \
  plot/draw/bar.m \
  plot/draw/colorbar.m \
  plot/draw/comet3.m \
  plot/draw/comet.m \
  plot/draw/compass.m \
  plot/draw/contour3.m \
  plot/draw/contourc.m \
  plot/draw/contourf.m \
  plot/draw/contour.m \
  plot/draw/cylinder.m \
  plot/draw/ellipsoid.m \
  plot/draw/errorbar.m \
  plot/draw/ezcontourf.m \
  plot/draw/ezcontour.m \
  plot/draw/ezmeshc.m \
  plot/draw/ezmesh.m \
  plot/draw/ezplot3.m \
  plot/draw/ezplot.m \
  plot/draw/ezpolar.m \
  plot/draw/ezsurfc.m \
  plot/draw/ezsurf.m \
  plot/draw/feather.m \
  plot/draw/fill.m \
  plot/draw/fplot.m \
  plot/draw/hist.m \
  plot/draw/isocolors.m \
  plot/draw/isonormals.m \
  plot/draw/isosurface.m \
  plot/draw/line.m \
  plot/draw/loglogerr.m \
  plot/draw/loglog.m \
  plot/draw/meshc.m \
  plot/draw/mesh.m \
  plot/draw/meshz.m \
  plot/draw/pareto.m \
  plot/draw/patch.m \
  plot/draw/pcolor.m \
  plot/draw/peaks.m \
  plot/draw/pie3.m \
  plot/draw/pie.m \
  plot/draw/plot3.m \
  plot/draw/plot.m \
  plot/draw/plotmatrix.m \
  plot/draw/plotyy.m \
  plot/draw/polar.m \
  plot/draw/quiver3.m \
  plot/draw/quiver.m \
  plot/draw/rectangle.m \
  plot/draw/ribbon.m \
  plot/draw/rose.m \
  plot/draw/scatter3.m \
  plot/draw/scatter.m \
  plot/draw/semilogxerr.m \
  plot/draw/semilogx.m \
  plot/draw/semilogyerr.m \
  plot/draw/semilogy.m \
  plot/draw/shrinkfaces.m \
  plot/draw/slice.m \
  plot/draw/sombrero.m \
  plot/draw/sphere.m \
  plot/draw/stairs.m \
  plot/draw/stem3.m \
  plot/draw/stemleaf.m \
  plot/draw/stem.m \
  plot/draw/surface.m \
  plot/draw/surfc.m \
  plot/draw/surfl.m \
  plot/draw/surf.m \
  plot/draw/surfnorm.m \
  plot/draw/tetramesh.m \
  plot/draw/trimesh.m \
  plot/draw/triplot.m \
  plot/draw/trisurf.m \
  plot/draw/waterfall.m \
  $(plot_draw_PRIVATE_FCN_FILES)

FCN_FILES += $(plot_draw_FCN_FILES)

PKG_ADD_FILES += plot/draw/PKG_ADD

DIRSTAMP_FILES += plot/draw/$(octave_dirstamp)


FCN_FILE_DIRS += plot/appearance

plot_appearance_PRIVATE_FCN_FILES = \
  plot/appearance/private/__axis_limits__.m \
  plot/appearance/private/__axis_label__.m

plot_appearance_FCN_FILES = \
  plot/appearance/__clabel__.m \
  plot/appearance/__getlegenddata__.m \
  plot/appearance/annotation.m \
  plot/appearance/axis.m \
  plot/appearance/box.m \
  plot/appearance/caxis.m \
  plot/appearance/clabel.m \
  plot/appearance/daspect.m \
  plot/appearance/datetick.m \
  plot/appearance/diffuse.m \
  plot/appearance/grid.m \
  plot/appearance/gtext.m \
  plot/appearance/hidden.m \
  plot/appearance/legend.m \
  plot/appearance/orient.m \
  plot/appearance/pbaspect.m \
  plot/appearance/shading.m \
  plot/appearance/specular.m \
  plot/appearance/text.m \
  plot/appearance/title.m \
  plot/appearance/view.m \
  plot/appearance/whitebg.m \
  plot/appearance/xlabel.m \
  plot/appearance/xlim.m \
  plot/appearance/ylabel.m \
  plot/appearance/ylim.m \
  plot/appearance/zlabel.m \
  plot/appearance/zlim.m \
  $(plot_appearance_PRIVATE_FCN_FILES)

FCN_FILES += $(plot_appearance_FCN_FILES)

PKG_ADD_FILES += plot/appearance/PKG_ADD

DIRSTAMP_FILES += plot/appearance/$(octave_dirstamp)


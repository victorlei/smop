FCN_FILE_DIRS += plot

plot_util_GEN_FCN_FILES = \
  plot/util/gnuplot_binary.m

GEN_FCN_FILES += $(plot_util_GEN_FCN_FILES)

plot_util_PRIVATE_FCN_FILES = \
  plot/util/private/__add_default_menu__.m \
  plot/util/private/__ghostscript__.m \
  plot/util/private/__gnuplot_get_var__.m \
  plot/util/private/__gnuplot_ginput__.m \
  plot/util/private/__gnuplot_has_feature__.m \
  plot/util/private/__gnuplot_has_terminal__.m \
  plot/util/private/__gnuplot_open_stream__.m \
  plot/util/private/__gnuplot_print__.m \
  plot/util/private/__gnuplot_version__.m \
  plot/util/private/__go_draw_axes__.m \
  plot/util/private/__go_draw_figure__.m \
  plot/util/private/__opengl_print__.m \
  plot/util/private/__print_parse_opts__.m \
  plot/util/private/__tight_eps_bbox__.m

plot_util_FCN_FILES = \
  plot/util/__actual_axis_position__.m \
  plot/util/allchild.m \
  plot/util/ancestor.m \
  plot/util/axes.m \
  plot/util/cla.m \
  plot/util/clf.m \
  plot/util/close.m \
  plot/util/closereq.m \
  plot/util/colstyle.m \
  plot/util/copyobj.m \
  plot/util/__default_plot_options__.m \
  plot/util/figure.m \
  plot/util/findall.m \
  plot/util/findfigs.m \
  plot/util/findobj.m \
  plot/util/frame2im.m \
  plot/util/gca.m \
  plot/util/gcbf.m \
  plot/util/gcbo.m \
  plot/util/gcf.m \
  plot/util/gco.m \
  plot/util/ginput.m \
  plot/util/__gnuplot_drawnow__.m \
  plot/util/graphics_toolkit.m \
  plot/util/hdl2struct.m \
  plot/util/hggroup.m \
  plot/util/hgload.m \
  plot/util/hgsave.m \
  plot/util/hold.m \
  plot/util/im2frame.m \
  plot/util/isaxes.m \
  plot/util/isfigure.m \
  plot/util/ishghandle.m \
  plot/util/ishold.m \
  plot/util/isprop.m \
  plot/util/linkaxes.m \
  plot/util/linkprop.m \
  plot/util/meshgrid.m \
  plot/util/ndgrid.m \
  plot/util/newplot.m \
  plot/util/__next_line_color__.m \
  plot/util/__next_line_style__.m \
  plot/util/pan.m \
  plot/util/__plt_get_axis_arg__.m \
  plot/util/__pltopt__.m \
  plot/util/printd.m \
  plot/util/print.m \
  plot/util/refreshdata.m \
  plot/util/refresh.m \
  plot/util/rotate.m \
  plot/util/rotate3d.m \
  plot/util/saveas.m \
  plot/util/shg.m \
  plot/util/struct2hdl.m \
  plot/util/subplot.m \
  plot/util/zoom.m \
  $(plot_util_PRIVATE_FCN_FILES)

FCN_FILES += $(plot_util_FCN_FILES)

PKG_ADD_FILES += plot/util/PKG_ADD

DIRSTAMP_FILES += plot/util/$(octave_dirstamp)


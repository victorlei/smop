EXTRA_DIST += \
  graphics/module.mk \
  graphics/qthandles.qrc \
  graphics/images/README \
  graphics/images/pan.png \
  graphics/images/rotate.png \
  graphics/images/select.png \
  graphics/images/zoom-in.png \
  graphics/images/zoom-out.png \
  $(octave_gui_graphics_UI)

octave_gui_MOC += \
  graphics/moc-annotation-dialog.cc \
  graphics/moc-Backend.cc \
  graphics/moc-ButtonControl.cc \
  graphics/moc-ContextMenu.cc \
  graphics/moc-EditControl.cc \
  graphics/moc-Figure.cc \
  graphics/moc-FigureWindow.cc \
  graphics/moc-ListBoxControl.cc \
  graphics/moc-Menu.cc \
  graphics/moc-MouseModeActionGroup.cc \
  graphics/moc-Object.cc \
  graphics/moc-ObjectFactory.cc \
  graphics/moc-ObjectProxy.cc \
  graphics/moc-PopupMenuControl.cc \
  graphics/moc-PushTool.cc \
  graphics/moc-SliderControl.cc \
  graphics/moc-TextEdit.cc \
  graphics/moc-ToggleTool.cc \
  graphics/moc-ToolBar.cc

octave_gui_graphics_UI = \
  graphics/annotation-dialog.ui

octave_gui_graphics_UI_H = $(patsubst graphics/%.ui, graphics/ui-%.h, $(octave_gui_graphics_UI))

BUILT_SOURCES += $(octave_gui_graphics_UI_H)

octave_gui_graphics_RC = graphics/qrc-qthandles.cc

noinst_HEADERS += \
  graphics/__init_qt__.h \
  graphics/annotation-dialog.h \
  graphics/Backend.h \
  graphics/BaseControl.h \
  graphics/ButtonControl.h \
  graphics/Canvas.h \
  graphics/CheckBoxControl.h \
  graphics/Container.h \
  graphics/ContextMenu.h \
  graphics/EditControl.h \
  graphics/Figure.h \
  graphics/FigureWindow.h \
  graphics/GenericEventNotify.h \
  graphics/GLCanvas.h \
  graphics/KeyMap.h \
  graphics/ListBoxControl.h \
  graphics/Logger.h \
  graphics/Menu.h \
  graphics/MenuContainer.h \
  graphics/MouseModeActionGroup.h \
  graphics/Object.h \
  graphics/ObjectFactory.h \
  graphics/ObjectProxy.h \
  graphics/Panel.h \
  graphics/PopupMenuControl.h \
  graphics/PushButtonControl.h \
  graphics/PushTool.h \
  graphics/QtHandlesUtils.h \
  graphics/RadioButtonControl.h \
  graphics/SliderControl.h \
  graphics/TextControl.h \
  graphics/TextEdit.h \
  graphics/ToggleButtonControl.h \
  graphics/ToggleTool.h \
  graphics/ToolBar.h \
  graphics/ToolBarButton.h \
  graphics/gl-select.h \
  $(TEMPLATE_SRC)

graphics_libgui_graphics_la_SOURCES = \
  graphics/__init_qt__.cc \
  graphics/annotation-dialog.cc \
  graphics/Backend.cc \
  graphics/BaseControl.cc \
  graphics/ButtonControl.cc \
  graphics/Canvas.cc \
  graphics/CheckBoxControl.cc \
  graphics/Container.cc \
  graphics/ContextMenu.cc \
  graphics/EditControl.cc \
  graphics/Figure.cc \
  graphics/FigureWindow.cc \
  graphics/GLCanvas.cc \
  graphics/KeyMap.cc \
  graphics/ListBoxControl.cc \
  graphics/Logger.cc \
  graphics/Menu.cc \
  graphics/MouseModeActionGroup.cc \
  graphics/Object.cc \
  graphics/ObjectFactory.cc \
  graphics/ObjectProxy.cc \
  graphics/Panel.cc \
  graphics/PopupMenuControl.cc \
  graphics/PushButtonControl.cc \
  graphics/PushTool.cc \
  graphics/QtHandlesUtils.cc \
  graphics/RadioButtonControl.cc \
  graphics/SliderControl.cc \
  graphics/TextControl.cc \
  graphics/TextEdit.cc \
  graphics/ToggleButtonControl.cc \
  graphics/ToggleTool.cc \
  graphics/ToolBar.cc \
  graphics/gl-select.cc

TEMPLATE_SRC = \
  graphics/ToolBarButton.cc

nodist_graphics_libgui_graphics_la_SOURCES = $(octave_gui_graphics_MOC) $(octave_gui_graphics_RC)

graphics_libgui_graphics_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_CPPFLAGS@ \
  -Igraphics -I$(srcdir)/graphics \
  -Isrc -I$(srcdir)/src \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_builddir)/liboctave/numeric -I$(top_srcdir)/liboctave/numeric \
  -I$(top_builddir)/liboctave/operators -I$(top_srcdir)/liboctave/operators \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_builddir)/libinterp -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/parse-tree -I$(top_srcdir)/libinterp/parse-tree \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_srcdir)/libinterp/octave-value

graphics_libgui_graphics_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

graphics_libgui_graphics_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

noinst_LTLIBRARIES += graphics/libgui-graphics.la

CLEANFILES += \
  $(octave_gui_graphics_MOC) \
  $(octave_gui_graphics_RC) \
  $(octave_gui_graphics_UI_H)

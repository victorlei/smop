EXTRA_DIST += \
  src/module.mk \
  src/resource.qrc \
  $(octave_gui_UI) \
  $(octave_gui_ICONS)

octave_gui_ICONS = \
  src/icons/applications-system.png \
  src/icons/bp-next.png \
  src/icons/bp-prev.png \
  src/icons/bp-rm-all.png \
  src/icons/bp-toggle.png \
  src/icons/db-cont.png \
  src/icons/db-step-in.png \
  src/icons/db-step-out.png \
  src/icons/db-step.png \
  src/icons/db-stop.png \
  src/icons/document-new.png \
  src/icons/document-open.png \
  src/icons/document-print.png \
  src/icons/document-save.png \
  src/icons/document-save-as.png \
  src/icons/edit-copy.png \
  src/icons/edit-cut.png \
  src/icons/edit-delete.png \
  src/icons/edit-find.png \
  src/icons/edit-find-replace.png \
  src/icons/edit-paste.png \
  src/icons/edit-redo.png \
  src/icons/edit-undo.png \
  src/icons/folder.png \
  src/icons/folder-new.png \
  src/icons/go-first.png \
  src/icons/go-last.png \
  src/icons/go-up.png \
  src/icons/graphic_logo_DocumentationDockWidget.png \
  src/icons/graphic_logo_FileEditor.png \
  src/icons/graphic_logo_FilesDockWidget.png \
  src/icons/graphic_logo_HistoryDockWidget.png \
  src/icons/graphic_logo_NewsDockWidget.png \
  src/icons/graphic_logo_TerminalDockWidget.png \
  src/icons/graphic_logo_WorkspaceView.png \
  src/icons/graphic_logo_ReleaseWidget.png \
  src/icons/icons_license \
  src/icons/letter_logo_DocumentationDockWidget.png \
  src/icons/letter_logo_FileEditor.png \
  src/icons/letter_logo_FilesDockWidget.png \
  src/icons/letter_logo_HistoryDockWidget.png \
  src/icons/letter_logo_NewsDockWidget.png \
  src/icons/letter_logo_TerminalDockWidget.png \
  src/icons/letter_logo_WorkspaceView.png \
  src/icons/letter_logo_ReleaseWidget.png \
  src/icons/logo.png \
  src/icons/preferences-system.png \
  src/icons/system-run.png \
  src/icons/user-home.png \
  src/icons/view-refresh.png \
  src/icons/widget-close.png \
  src/icons/widget-dock.png \
  src/icons/widget-undock.png \
  src/icons/widget-close-light.png \
  src/icons/widget-dock-light.png \
  src/icons/widget-undock-light.png \
  src/icons/zoom-in.png \
  src/icons/zoom-out.png

octave_gui_MOC =

if AMCOND_HAVE_QSCINTILLA
octave_gui_MOC += \
  src/m-editor/moc-file-editor-interface.cc \
  src/m-editor/moc-file-editor-tab.cc \
  src/m-editor/moc-file-editor.cc \
  src/m-editor/moc-find-dialog.cc \
  src/m-editor/moc-octave-qscintilla.cc \
  src/m-editor/moc-octave-txt-lexer.cc
endif

octave_gui_MOC += \
  src/moc-dialog.cc \
  src/moc-documentation-dock-widget.cc \
  src/moc-files-dock-widget.cc \
  src/moc-history-dock-widget.cc \
  src/moc-main-window.cc \
  src/moc-octave-interpreter.cc \
  src/moc-octave-qt-link.cc \
  src/moc-settings-dialog.cc \
  src/moc-terminal-dock-widget.cc \
  src/moc-color-picker.cc \
  src/moc-resource-manager.cc \
  src/moc-shortcut-manager.cc \
  src/moc-welcome-wizard.cc \
  src/moc-workspace-model.cc \
  src/moc-workspace-view.cc \
  src/moc-find-files-dialog.cc \
  src/moc-find-files-model.cc \
  src/qtinfo/moc-parser.cc \
  src/qtinfo/moc-webinfo.cc \
  src/moc-octave-dock-widget.cc

octave_gui_RC = src/qrc-resource.cc

octave_gui_UI = \
  src/settings-dialog.ui

octave_gui_UI_H = $(patsubst src/%.ui, src/ui-%.h, $(octave_gui_UI))

BUILT_SOURCES += $(octave_gui_UI_H)

noinst_HEADERS += \
  src/dialog.h \
  src/octave-dock-widget.h \
  src/documentation-dock-widget.h \
  src/files-dock-widget.h \
  src/history-dock-widget.h \
  src/m-editor/file-editor-interface.h \
  src/m-editor/file-editor-tab.h \
  src/m-editor/file-editor.h \
  src/m-editor/find-dialog.h \
  src/m-editor/octave-qscintilla.h \
  src/m-editor/octave-txt-lexer.h \
  src/main-window.h \
  src/octave-gui.h \
  src/octave-cmd.h \
  src/octave-interpreter.h \
  src/octave-qt-link.h \
  src/qtinfo/parser.h \
  src/qtinfo/webinfo.h \
  src/resource-manager.h \
  src/settings-dialog.h \
  src/shortcut-manager.h \
  src/thread-manager.h \
  src/terminal-dock-widget.h \
  src/color-picker.h \
  src/welcome-wizard.h \
  src/find-files-dialog.h \
  src/find-files-model.h \
  src/workspace-model.h \
  src/workspace-view.h

src_libgui_src_la_SOURCES = \
  src/dialog.cc \
  src/documentation-dock-widget.cc \
  src/files-dock-widget.cc \
  src/history-dock-widget.cc \
  src/m-editor/file-editor-tab.cc \
  src/m-editor/file-editor.cc \
  src/m-editor/find-dialog.cc \
  src/m-editor/octave-qscintilla.cc \
  src/m-editor/octave-txt-lexer.cc \
  src/main-window.cc \
  src/octave-cmd.cc \
  src/octave-dock-widget.cc \
  src/octave-gui.cc \
  src/octave-interpreter.cc \
  src/octave-qt-link.cc \
  src/qtinfo/parser.cc \
  src/qtinfo/webinfo.cc \
  src/resource-manager.cc \
  src/settings-dialog.cc \
  src/shortcut-manager.cc \
  src/thread-manager.cc \
  src/terminal-dock-widget.cc \
  src/color-picker.cc \
  src/welcome-wizard.cc \
  src/find-files-dialog.cc \
  src/find-files-model.cc \
  src/workspace-model.cc \
  src/workspace-view.cc

nodist_src_libgui_src_la_SOURCES = $(octave_gui_MOC) $(octave_gui_RC)

src_libgui_src_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_CPPFLAGS@ \
  -I$(srcdir)/qterminal/libqterminal \
  -Isrc -I$(srcdir)/src \
  -I$(srcdir)/src/m-editor \
  -I$(srcdir)/src/qtinfo \
  -I$(srcdir)/graphics \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_builddir)/liboctave/numeric -I$(top_srcdir)/liboctave/numeric \
  -I$(top_builddir)/liboctave/operators -I$(top_srcdir)/liboctave/operators \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_builddir)/libinterp -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/parse-tree -I$(top_srcdir)/libinterp/parse-tree \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_srcdir)/libinterp/octave-value \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

src_libgui_src_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

src_libgui_src_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

noinst_LTLIBRARIES += src/libgui-src.la

CLEANFILES += \
  $(octave_gui_MOC) \
  $(octave_gui_UI_H) \
  $(octave_gui_RC)


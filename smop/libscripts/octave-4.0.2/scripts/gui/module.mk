FCN_FILE_DIRS += gui

gui_PRIVATE_FCN_FILES = \
  gui/private/__file_filter__.m \
  gui/private/__fltk_file_filter__.m \
  gui/private/__get_funcname__.m \
  gui/private/__is_function__.m \
  gui/private/__uigetdir_fltk__.m \
  gui/private/__uigetfile_fltk__.m \
  gui/private/__uiobject_split_args__.m \
  gui/private/__uiputfile_fltk__.m \
  gui/private/message_dialog.m

gui_FCN_FILES = \
  gui/errordlg.m \
  gui/guidata.m \
  gui/guihandles.m \
  gui/helpdlg.m \
  gui/inputdlg.m \
  gui/listdlg.m \
  gui/msgbox.m \
  gui/questdlg.m \
  gui/uicontextmenu.m \
  gui/uicontrol.m \
  gui/uigetdir.m \
  gui/uigetfile.m \
  gui/uimenu.m \
  gui/uipanel.m \
  gui/uipushtool.m \
  gui/uiputfile.m \
  gui/uiresume.m \
  gui/uitoggletool.m \
  gui/uitoolbar.m \
  gui/uiwait.m \
  gui/waitbar.m \
  gui/waitforbuttonpress.m \
  gui/warndlg.m \
  $(gui_PRIVATE_FCN_FILES)

FCN_FILES += $(gui_FCN_FILES)

PKG_ADD_FILES += gui/PKG_ADD

DIRSTAMP_FILES += gui/$(octave_dirstamp)

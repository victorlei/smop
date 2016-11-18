/*

Copyright (C) 2011-2015 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "resource-manager.h"
#include "shortcut-manager.h"
#include "workspace-model.h"
#include "settings-dialog.h"
#include "ui-settings-dialog.h"
#include <QDir>
#include <QFileInfo>
#include <QFileDialog>
#include <QVector>
#include <QHash>

#ifdef HAVE_QSCINTILLA
#include "octave-qscintilla.h"
#include "octave-txt-lexer.h"
#include <QScrollArea>

#if defined (HAVE_QSCI_QSCILEXEROCTAVE_H)
#define HAVE_LEXER_OCTAVE
#include <Qsci/qscilexeroctave.h>
#elif defined (HAVE_QSCI_QSCILEXERMATLAB_H)
#define HAVE_LEXER_MATLAB
#include <Qsci/qscilexermatlab.h>
#endif

#include <Qsci/qscilexercpp.h>
#include <Qsci/qscilexerbash.h>
#include <Qsci/qscilexerperl.h>
#include <Qsci/qscilexerbatch.h>
#include <Qsci/qscilexerdiff.h>
#endif

settings_dialog::settings_dialog (QWidget *p, const QString& desired_tab):
  QDialog (p), ui (new Ui::settings_dialog)
{
  ui->setupUi (this);

  QSettings *settings = resource_manager::get_settings ();

  // restore last geometry
  restoreGeometry (settings->value("settings/geometry").toByteArray ());

  // look for available language files and the actual settings
  QString qm_dir_name = resource_manager::get_gui_translation_dir ();
  QDir qm_dir (qm_dir_name);
  QFileInfoList qm_files = qm_dir.entryInfoList (QStringList ("*.qm"),
                                                 QDir::Files | QDir::Readable,
                                                 QDir::Name);
  for (int i = 0; i < qm_files.length (); i++)   // insert available languages
    ui->comboBox_language->addItem (qm_files.at (i).baseName ());
  // System at beginning
  ui->comboBox_language->insertItem (0,tr ("System setting"));
  ui->comboBox_language->insertSeparator (1);    // separator after System
  QString language = settings->value ("language","SYSTEM").toString ();
  if (language == "SYSTEM")
    language = tr ("System setting");
  int selected = ui->comboBox_language->findText (language);
  if (selected >= 0)
    ui->comboBox_language->setCurrentIndex (selected);
  else
    ui->comboBox_language->setCurrentIndex (0);  // System is default

  // icon size
  QButtonGroup *icon_size_group = new QButtonGroup (this);
  icon_size_group->addButton (ui->icon_size_small);
  icon_size_group->addButton (ui->icon_size_normal);
  icon_size_group->addButton (ui->icon_size_large);
  int icon_size = settings->value ("toolbar_icon_size", 0).toInt ();
  ui->icon_size_normal->setChecked (true);  // the default
  ui->icon_size_small->setChecked (icon_size == -1);
  ui->icon_size_large->setChecked (icon_size == 1);

  // which icon has to be selected
  QButtonGroup *icon_group = new QButtonGroup (this);
  icon_group->addButton (ui->general_icon_octave);
  icon_group->addButton (ui->general_icon_graphic);
  icon_group->addButton (ui->general_icon_letter);
  QString widget_icon_set =
    settings->value ("DockWidgets/widget_icon_set","NONE").toString ();
  ui->general_icon_octave-> setChecked (true);  // the default (if invalid set)
  ui->general_icon_octave-> setChecked (widget_icon_set == "NONE");
  ui->general_icon_graphic-> setChecked (widget_icon_set == "GRAPHIC");
  ui->general_icon_letter-> setChecked (widget_icon_set == "LETTER");

  // custom title bar of dock widget
  QVariant default_var = QColor (255,255,255);
  QColor bg_color = settings->value ("Dockwidgets/title_bg_color",
                                     default_var).value<QColor> ();
  _widget_title_bg_color = new color_picker (bg_color);
  _widget_title_bg_color->setEnabled (false);
  ui->layout_widget_bgtitle->addWidget (_widget_title_bg_color,0);
  connect (ui->cb_widget_custom_style, SIGNAL (toggled (bool)),
           _widget_title_bg_color, SLOT (setEnabled (bool)));

  default_var = QColor (192,192,192);
  QColor bg_color_active = settings->value ("Dockwidgets/title_bg_color_active",
                                      default_var).value<QColor> ();
  _widget_title_bg_color_active = new color_picker (bg_color_active);
  _widget_title_bg_color_active->setEnabled (false);
  ui->layout_widget_bgtitle_active->addWidget (_widget_title_bg_color_active,0);
  connect (ui->cb_widget_custom_style, SIGNAL (toggled (bool)),
           _widget_title_bg_color_active, SLOT (setEnabled (bool)));

  default_var = QColor (0,0,0);
  QColor fg_color = settings->value ("Dockwidgets/title_fg_color",
                                     default_var).value<QColor> ();
  _widget_title_fg_color = new color_picker (fg_color);
  _widget_title_fg_color->setEnabled (false);
  ui->layout_widget_fgtitle->addWidget (_widget_title_fg_color,0);
  connect (ui->cb_widget_custom_style, SIGNAL (toggled (bool)),
           _widget_title_fg_color, SLOT (setEnabled (bool)));

  default_var = QColor (0,0,0);
  QColor fg_color_active = settings->value ("Dockwidgets/title_fg_color_active",
                                      default_var).value<QColor> ();
  _widget_title_fg_color_active = new color_picker (fg_color_active);
  _widget_title_fg_color_active->setEnabled (false);
  ui->layout_widget_fgtitle_active->addWidget (_widget_title_fg_color_active,0);
  connect (ui->cb_widget_custom_style, SIGNAL (toggled (bool)),
           _widget_title_fg_color_active, SLOT (setEnabled (bool)));

  ui->sb_3d_title->setValue (
    settings->value ("DockWidgets/widget_title_3d", 50).toInt ());
  ui->cb_widget_custom_style->setChecked (
    settings->value ("DockWidgets/widget_title_custom_style",false).toBool ());

  // prompt on exit
  ui->cb_prompt_to_exit->setChecked (
    settings->value ("prompt_to_exit",false).toBool ());

  // Main status bar
  ui->cb_status_bar->setChecked (
    settings->value ("show_status_bar",true).toBool ());

  // Octave startup
  ui->cb_restore_octave_dir->setChecked (
    settings->value ("restore_octave_dir",false).toBool ());
  ui->le_octave_dir->setText (
    settings->value ("octave_startup_dir").toString ());
  connect (ui->pb_octave_dir, SIGNAL (pressed ()),
           this, SLOT (get_octave_dir ()));

  // editor
  ui->useCustomFileEditor->setChecked (settings->value ("useCustomFileEditor",
                                                        false).toBool ());
  ui->customFileEditor->setText (
    settings->value ("customFileEditor").toString ());
  ui->editor_showLineNumbers->setChecked (
    settings->value ("editor/showLineNumbers",true).toBool ());

  default_var = QColor (240, 240, 240);
  QColor setting_color = settings->value ("editor/highlight_current_line_color",
                                          default_var).value<QColor> ();
  _editor_current_line_color = new color_picker (setting_color);
  ui->editor_grid_current_line->addWidget (_editor_current_line_color,0,3);
  _editor_current_line_color->setMinimumSize (50,10);
  _editor_current_line_color->setEnabled (false);
  connect (ui->editor_highlightCurrentLine, SIGNAL (toggled (bool)),
           _editor_current_line_color, SLOT (setEnabled (bool)));
  ui->editor_highlightCurrentLine->setChecked (
    settings->value ("editor/highlightCurrentLine",true).toBool ());
  ui->editor_long_line_marker->setChecked (
    settings->value ("editor/long_line_marker",true).toBool ());
  ui->editor_long_line_column->setValue (
    settings->value ("editor/long_line_column",80).toInt ());
  ui->cb_edit_status_bar->setChecked (
    settings->value ("editor/show_edit_status_bar",true).toBool ());
  ui->cb_code_folding->setChecked (
    settings->value ("editor/code_folding",true).toBool ());

  ui->editor_codeCompletion->setChecked (
    settings->value ("editor/codeCompletion", true).toBool ());
  ui->editor_spinbox_ac_threshold->setValue (
    settings->value ("editor/codeCompletion_threshold",2).toInt ());
  ui->editor_checkbox_ac_keywords->setChecked (
    settings->value ("editor/codeCompletion_keywords",true).toBool ());
  ui->editor_checkbox_ac_builtins->setEnabled (
    ui->editor_checkbox_ac_keywords->isChecked ());
  ui->editor_checkbox_ac_functions->setEnabled (
    ui->editor_checkbox_ac_keywords->isChecked ());
  ui->editor_checkbox_ac_builtins->setChecked (
    settings->value ("editor/codeCompletion_octave_builtins",true).toBool ());
  ui->editor_checkbox_ac_functions->setChecked (
    settings->value ("editor/codeCompletion_octave_functions",true).toBool ());
  ui->editor_checkbox_ac_document->setChecked (
    settings->value ("editor/codeCompletion_document",false).toBool ());
  ui->editor_checkbox_ac_case->setChecked (
    settings->value ("editor/codeCompletion_case",true).toBool ());
  ui->editor_checkbox_ac_replace->setChecked (
    settings->value ("editor/codeCompletion_replace",false).toBool ());
  ui->editor_ws_checkbox->setChecked (
    settings->value ("editor/show_white_space", false).toBool ());
  ui->editor_ws_indent_checkbox->setChecked (
    settings->value ("editor/show_white_space_indent",false).toBool ());
  ui->cb_show_eol->setChecked (
    settings->value ("editor/show_eol_chars",false).toBool () );
  ui->cb_show_hscrollbar->setChecked (
    settings->value ("editor/show_hscroll_bar",true).toBool ());

#ifdef HAVE_QSCINTILLA
#if defined (Q_OS_WIN32)
  int eol_mode = QsciScintilla::EolWindows;
#elif defined (Q_OS_MAC)
  int eol_mode = QsciScintilla::EolMac;
#else
  int eol_mode = QsciScintilla::EolUnix;
#endif
#else
  int eol_mode = 2;
#endif
  ui->combo_eol_mode->setCurrentIndex (
    settings->value ("editor/default_eol_mode",eol_mode).toInt () );
  ui->editor_auto_ind_checkbox->setChecked (
    settings->value ("editor/auto_indent", true).toBool ());
  ui->editor_tab_ind_checkbox->setChecked (
    settings->value ("editor/tab_indents_line",false).toBool ());
  ui->editor_bs_unind_checkbox->setChecked (
    settings->value ("editor/backspace_unindents_line",false).toBool ());
  ui->editor_ind_guides_checkbox->setChecked (
    settings->value ("editor/show_indent_guides",false).toBool ());
  ui->editor_ind_width_spinbox->setValue (
    settings->value ("editor/indent_width", 2).toInt ());
  ui->editor_ind_uses_tabs_checkbox->setChecked (
    settings->value ("editor/indent_uses_tabs", false).toBool ());
  ui->editor_tab_width_spinbox->setValue (
    settings->value ("editor/tab_width", 2).toInt ());
  ui->editor_longWindowTitle->setChecked (
    settings->value ("editor/longWindowTitle",false).toBool ());
  ui->editor_notebook_tab_width_min->setValue (
    settings->value ("editor/notebook_tab_width_min", 160).toInt ());
  ui->editor_notebook_tab_width_max->setValue (
    settings->value ("editor/notebook_tab_width_max", 300).toInt ());
  ui->editor_restoreSession->setChecked (
    settings->value ("editor/restoreSession", true).toBool ());
  ui->editor_create_new_file->setChecked (
    settings->value ("editor/create_new_file",false).toBool ());
  ui->editor_reload_changed_files->setChecked (
    settings->value ("editor/always_reload_changed_files",false).toBool ());

  // terminal
  ui->terminal_fontName->setCurrentFont (QFont (
      settings->value ("terminal/fontName","Courier New").toString ()));
  ui->terminal_fontSize->setValue (
    settings->value ("terminal/fontSize", 10).toInt ());
  ui->terminal_history_buffer->setValue (
    settings->value ("terminal/history_buffer",1000).toInt ());
  ui->terminal_cursorBlinking->setChecked (
    settings->value ("terminal/cursorBlinking",true).toBool ());
  ui->terminal_cursorUseForegroundColor->setChecked (
    settings->value ("terminal/cursorUseForegroundColor",true).toBool ());
  ui->terminal_focus_command->setChecked (
    settings->value ("terminal/focus_after_command",false).toBool ());
  ui->terminal_print_dbg_location->setChecked (
    settings->value ("terminal/print_debug_location",false).toBool ());

  QString cursorType
    = settings->value ("terminal/cursorType", "ibeam").toString ();

  QStringList items;
  items << QString ("0") << QString ("1") << QString ("2");
  ui->terminal_cursorType->addItems (items);
  ui->terminal_cursorType->setItemText (0, tr ("IBeam Cursor"));
  ui->terminal_cursorType->setItemText (1, tr ("Block Cursor"));
  ui->terminal_cursorType->setItemText (2, tr ("Underline Cursor"));

  if (cursorType == "ibeam")
    ui->terminal_cursorType->setCurrentIndex (0);
  else if (cursorType == "block")
    ui->terminal_cursorType->setCurrentIndex (1);
  else if (cursorType == "underline")
    ui->terminal_cursorType->setCurrentIndex (2);

  // file browser
  ui->showFileSize->setChecked (
    settings->value ("filesdockwidget/showFileSize", false).toBool ());
  ui->showFileType->setChecked (
    settings->value ("filesdockwidget/showFileType", false).toBool ());
  ui->showLastModified->setChecked (
    settings->value ("filesdockwidget/showLastModified",false).toBool ());
  ui->showHiddenFiles->setChecked (
    settings->value ("filesdockwidget/showHiddenFiles",false).toBool ());
  ui->useAlternatingRowColors->setChecked (
    settings->value ("filesdockwidget/useAlternatingRowColors",true).toBool ());
  connect (ui->sync_octave_directory, SIGNAL (toggled (bool)),
           this, SLOT (set_disabled_pref_file_browser_dir (bool)));
  ui->sync_octave_directory->setChecked (
    settings->value ("filesdockwidget/sync_octave_directory",true).toBool ());
  ui->cb_restore_file_browser_dir->setChecked (
    settings->value ("filesdockwidget/restore_last_dir",false).toBool ());
  ui->le_file_browser_dir->setText (
    settings->value ("filesdockwidget/startup_dir").toString ());
  connect (ui->pb_file_browser_dir, SIGNAL (pressed ()),
           this, SLOT (get_file_browser_dir ()));

  ui->checkbox_allow_web_connect->setChecked (
    settings->value ("news/allow_web_connection",false).toBool ());
  ui->useProxyServer->setChecked (
    settings->value ("useProxyServer", false).toBool ());
  ui->proxyHostName->setText (settings->value ("proxyHostName").toString ());

  int currentIndex = 0;
  QString proxyTypeString = settings->value ("proxyType").toString ();
  while ((currentIndex < ui->proxyType->count ())
         && (ui->proxyType->currentText () != proxyTypeString))
    {
      currentIndex++;
      ui->proxyType->setCurrentIndex (currentIndex);
    }

  ui->proxyPort->setText (settings->value ("proxyPort").toString ());
  ui->proxyUserName->setText (settings->value ("proxyUserName").toString ());
  ui->proxyPassword->setText (settings->value ("proxyPassword").toString ());

  // Workspace
  // colors
  read_workspace_colors (settings);
  // hide tool tips
  ui->cb_hide_tool_tips->setChecked (
    settings->value ("workspaceview/hide_tool_tips",false).toBool ());

  // terminal colors
  read_terminal_colors (settings);

  // shortcuts

  ui->cb_prevent_readline_conflicts->setChecked (
    settings->value ("shortcuts/prevent_readline_conflicts", true).toBool ());
  int set = settings->value ("shortcuts/set",0).toInt ();
  ui->rb_sc_set1->setChecked (set == 0);
  ui->rb_sc_set2->setChecked (set == 1);

  // initialize the tree view with all shortcut data
  shortcut_manager::fill_treewidget (ui->shortcuts_treewidget);

  // connect the buttons for import/export of the shortcut sets
  connect (ui->btn_import_shortcut_set1, SIGNAL (clicked ()),
           this, SLOT (import_shortcut_set1 ()));
  connect (ui->btn_export_shortcut_set1, SIGNAL (clicked ()),
           this, SLOT (export_shortcut_set1 ()));
  connect (ui->btn_import_shortcut_set2, SIGNAL (clicked ()),
           this, SLOT (import_shortcut_set2 ()));
  connect (ui->btn_export_shortcut_set2, SIGNAL (clicked ()),
           this, SLOT (export_shortcut_set2 ()));


#ifdef HAVE_QSCINTILLA
  // editor styles: create lexer, read settings, and create dialog elements
  QsciLexer *lexer;
#if defined (HAVE_LEXER_OCTAVE)
  lexer = new QsciLexerOctave ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#elif defined (HAVE_LEXER_MATLAB)
  lexer = new QsciLexerMatlab ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#endif
  lexer = new QsciLexerCPP ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerPerl ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBatch ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerDiff ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBash ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new octave_txt_lexer ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#endif

  // which tab is the desired one?
  show_tab (desired_tab);

  // connect button box signal
  connect (ui->button_box, SIGNAL (clicked (QAbstractButton *)),
           this,           SLOT (button_clicked (QAbstractButton *)));
}

settings_dialog::~settings_dialog ()
{
  delete ui;
}

void
settings_dialog::show_tab (const QString& tab)
{
  if (tab.isEmpty ())
    {
      QSettings *settings = resource_manager::get_settings ();
      ui->tabWidget->setCurrentIndex (settings->value ("settings/last_tab",
                                      0).toInt ());
    }
  else
    {
      QHash <QString, QWidget*> tab_hash;
      tab_hash["editor"] = ui->tab_editor;
      tab_hash["editor_styles"] = ui->tab_editor_styles;
      ui->tabWidget->setCurrentIndex (
        ui->tabWidget->indexOf (tab_hash.value (tab)));
    }
}

#ifdef HAVE_QSCINTILLA
int
settings_dialog::get_valid_lexer_styles (QsciLexer *lexer, int styles[])
{
  int max_style = 0;
  int actual_style = 0;
  while (actual_style < MaxStyleNumber && max_style < MaxLexerStyles)
    {
      if ((lexer->description (actual_style)) != "")  // valid style
        styles[max_style++] = actual_style;
      actual_style++;
    }
  return max_style;
}

void
settings_dialog::read_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  lexer->readSettings (*settings);
  int styles[MaxLexerStyles];  // array for saving valid styles
                               // (enum is not continuous)
  int max_style = get_valid_lexer_styles (lexer, styles);
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (max_style);
  QVector<QFontComboBox*> select_font (max_style);
  QVector<QSpinBox*> font_size (max_style);
  QVector<QCheckBox*> attrib_font (3 * max_style);
  QVector<color_picker*> color (max_style);
  QVector<color_picker*> bg_color (max_style);
  int default_size = 10;
  QFont default_font = QFont ();
  int label_width;
  QColor default_color = QColor ();
  QColor dummy_color = QColor (255,0,255);

  for (int i = 0; i < max_style; i++)  // create dialog elements for all styles
    {
      QString actual_name = lexer->description (styles[i]);
      QFont   actual_font = lexer->font (styles[i]);
      description[i] = new QLabel (actual_name);
      description[i]->setWordWrap (true);
      label_width = 24*description[i]->fontMetrics ().averageCharWidth ();
      description[i]->setMaximumSize (label_width,QWIDGETSIZE_MAX);
      description[i]->setMinimumSize (label_width,1);
      select_font[i] = new QFontComboBox ();
      select_font[i]->setObjectName (actual_name+"_font");
      select_font[i]->setMaximumSize (label_width,QWIDGETSIZE_MAX);
      select_font[i]->setMinimumSize (label_width,1);
      font_size[i] = new QSpinBox ();
      font_size[i]->setObjectName (actual_name+"_size");
      if (styles[i] == 0) // the default
        {
          select_font[i]->setCurrentFont (actual_font);
          default_font = actual_font;
          font_size[i]->setRange (6,24);
          default_size = actual_font.pointSize ();
          font_size[i]->setValue (default_size);
          default_color = lexer->defaultPaper ();
          bg_color[i] = new color_picker (default_color);
        }
      else   // other styles
        {
          select_font[i]->setCurrentFont (actual_font);
          if (actual_font.family () == default_font.family ())
            select_font[i]->setEditText (lexer->description (0));
          font_size[i]->setRange (-4,4);
          font_size[i]->setValue (actual_font.pointSize ()-default_size);
          font_size[i]->setToolTip (tr ("Difference to the default size"));
          if (lexer->paper (styles[i]) == default_color)
            bg_color[i] = new color_picker (dummy_color);
          else
            bg_color[i] = new color_picker (lexer->paper (styles[i]));
          bg_color[i]->setToolTip
          (tr ("Background color, pink (255,0,255) means default"));
        }
      attrib_font[0+3*i] = new QCheckBox (tr ("b", "short form for bold"));
      attrib_font[1+3*i] = new QCheckBox (tr ("i", "short form for italic"));
      attrib_font[2+3*i] = new QCheckBox (tr ("u", "short form for underlined"));
      attrib_font[0+3*i]->setChecked (Qt::Checked && actual_font.bold ());
      attrib_font[0+3*i]->setObjectName (actual_name+"_bold");
      attrib_font[1+3*i]->setChecked (Qt::Checked && actual_font.italic ());
      attrib_font[1+3*i]->setObjectName (actual_name+"_italic");
      attrib_font[2+3*i]->setChecked (Qt::Checked && actual_font.underline ());
      attrib_font[2+3*i]->setObjectName (actual_name+"_underline");
      color[i] = new color_picker (lexer->color (styles[i]));
      color[i]->setObjectName (actual_name+"_color");
      bg_color[i]->setObjectName (actual_name+"_bg_color");
      int column = 1;
      style_grid->addWidget (description[i],     i, column++);
      style_grid->addWidget (select_font[i],     i, column++);
      style_grid->addWidget (font_size[i],       i, column++);
      style_grid->addWidget (attrib_font[0+3*i], i, column++);
      style_grid->addWidget (attrib_font[1+3*i], i, column++);
      style_grid->addWidget (attrib_font[2+3*i], i, column++);
      style_grid->addWidget (color[i],           i, column++);
      style_grid->addWidget (bg_color[i],        i, column++);
    }
  // place grid with elements into the tab
  QScrollArea *scroll_area = new QScrollArea ();
  QWidget *scroll_area_contents = new QWidget ();
  scroll_area_contents->setObjectName (QString (lexer->language ())+"_styles");
  scroll_area_contents->setLayout (style_grid);
  scroll_area->setWidget (scroll_area_contents);
  ui->tabs_editor_lexers->addTab (scroll_area,lexer->language ());

  ui->tabs_editor_lexers->setCurrentIndex (
    settings->value ("settings/last_editor_styles_tab",0).toInt ());
}
#endif

void
settings_dialog::read_workspace_colors (QSettings *settings)
{

  QList<QColor> default_colors =
    resource_manager::storage_class_default_colors ();
  QStringList class_names = resource_manager::storage_class_names ();
  QString class_chars = resource_manager::storage_class_chars ();
  int nr_of_classes = class_chars.length ();

  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (nr_of_classes);
  QVector<color_picker*> color (nr_of_classes);

  int column = 0;
  int row = 0;
  for (int i = 0; i < nr_of_classes; i++)
    {
      description[i] = new QLabel ("    " + class_names.at (i));
      description[i]->setAlignment (Qt::AlignRight);
      QVariant default_var = default_colors.at (i);
      QColor setting_color = settings->value ("workspaceview/color_"
                                              + class_chars.mid (i,1),
                                              default_var).value<QColor> ();
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName ("color_"+class_chars.mid (i, 1));
      color[i]->setMinimumSize (30, 10);
      style_grid->addWidget (description[i], row, 3*column);
      style_grid->addWidget (color[i],       row, 3*column+1);
      if (++column == 3)
        {
          style_grid->setColumnStretch (4*column, 10);
          row++;
          column = 0;
        }
    }

  // place grid with elements into the tab
  ui->workspace_colors_box->setLayout (style_grid);
}

void
settings_dialog::read_terminal_colors (QSettings *settings)
{

  QList<QColor> default_colors = resource_manager::terminal_default_colors ();
  QStringList class_names = resource_manager::terminal_color_names ();
  QString class_chars = resource_manager::terminal_color_chars ();
  int nr_of_classes = class_chars.length ();

  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (nr_of_classes);
  QVector<color_picker*> color (nr_of_classes);

  int column = 0;
  int row = 0;
  for (int i = 0; i < nr_of_classes; i++)
    {
      description[i] = new QLabel ("    " + class_names.at (i));
      description[i]->setAlignment (Qt::AlignRight);
      QVariant default_var = default_colors.at (i);
      QColor setting_color = settings->value ("terminal/color_"
                                              + class_chars.mid (i,1),
                                              default_var).value<QColor> ();
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName ("terminal_color_"+class_chars.mid (i, 1));
      color[i]->setMinimumSize (30, 10);
      style_grid->addWidget (description[i], row, 2*column);
      style_grid->addWidget (color[i],       row, 2*column+1);
      if (++column == 2)
        {
          style_grid->setColumnStretch (3*column, 10);
          row++;
          column = 0;
        }
    }

  // place grid with elements into the tab
  ui->terminal_colors_box->setLayout (style_grid);
}

void
settings_dialog::write_changed_settings (bool closing)
{
  QSettings *settings = resource_manager::get_settings ();
  // FIXME: what should happen if settings is 0?

  // the icon set
  QString widget_icon_set = "NONE";
  if (ui->general_icon_letter->isChecked ())
    widget_icon_set = "LETTER";
  else if (ui->general_icon_graphic->isChecked ())
    widget_icon_set = "GRAPHIC";
  settings->setValue ("DockWidgets/widget_icon_set",widget_icon_set);

  // language
  QString language = ui->comboBox_language->currentText ();
  if (language == tr ("System setting"))
    language = "SYSTEM";
  settings->setValue ("language", language);

  // dock widget title bar
  settings->setValue ("DockWidgets/widget_title_custom_style",
                      ui->cb_widget_custom_style->isChecked ());
  settings->setValue ("DockWidgets/widget_title_3d",
                      ui->sb_3d_title->value ( ));
  settings->setValue ("Dockwidgets/title_bg_color",
                      _widget_title_bg_color->color ());
  settings->setValue ("Dockwidgets/title_bg_color_active",
                      _widget_title_bg_color_active->color ());
  settings->setValue ("Dockwidgets/title_fg_color",
                      _widget_title_fg_color->color ());
  settings->setValue ("Dockwidgets/title_fg_color_active",
                      _widget_title_fg_color_active->color ());

  // icon size
  int icon_size = 0;
  if (ui->icon_size_small->isChecked ())
    icon_size = -1;
  else if (ui->icon_size_large->isChecked ())
    icon_size = 1;
  settings->setValue ("toolbar_icon_size", icon_size);

  // promp to exit
  settings->setValue ("prompt_to_exit", ui->cb_prompt_to_exit->isChecked ());

  // status bar
  settings->setValue ( "show_status_bar", ui->cb_status_bar->isChecked ());

  // Octave startup
  settings->setValue ("restore_octave_dir",
                      ui->cb_restore_octave_dir->isChecked ());
  settings->setValue ("octave_startup_dir", ui->le_octave_dir->text ());

  //editor
  settings->setValue ("useCustomFileEditor",
                      ui->useCustomFileEditor->isChecked ());
  settings->setValue ("customFileEditor", ui->customFileEditor->text ());
  settings->setValue ("editor/showLineNumbers",
                      ui->editor_showLineNumbers->isChecked ());
  settings->setValue ("editor/highlightCurrentLine",
                      ui->editor_highlightCurrentLine->isChecked ());
  settings->setValue ("editor/highlight_current_line_color",
                      _editor_current_line_color->color ());
  settings->setValue ("editor/long_line_marker",
                      ui->editor_long_line_marker->isChecked ());
  settings->setValue ("editor/long_line_column",
                      ui->editor_long_line_column->value ());
  settings->setValue ("editor/code_folding",
                      ui->cb_code_folding->isChecked ());
  settings->setValue ("editor/show_edit_status_bar",
                      ui->cb_edit_status_bar->isChecked ());
  settings->setValue ("editor/codeCompletion",
                      ui->editor_codeCompletion->isChecked ());
  settings->setValue ("editor/codeCompletion_threshold",
                      ui->editor_spinbox_ac_threshold->value ());
  settings->setValue ("editor/codeCompletion_keywords",
                      ui->editor_checkbox_ac_keywords->isChecked ());
  settings->setValue ("editor/codeCompletion_octave_builtins",
                      ui->editor_checkbox_ac_builtins->isChecked ());
  settings->setValue ("editor/codeCompletion_octave_functions",
                      ui->editor_checkbox_ac_functions->isChecked ());
  settings->setValue ("editor/codeCompletion_document",
                      ui->editor_checkbox_ac_document->isChecked ());
  settings->setValue ("editor/codeCompletion_case",
                      ui->editor_checkbox_ac_case->isChecked ());
  settings->setValue ("editor/codeCompletion_replace",
                      ui->editor_checkbox_ac_replace->isChecked ());
  settings->setValue ("editor/show_white_space",
                      ui->editor_ws_checkbox->isChecked ());
  settings->setValue ("editor/show_white_space_indent",
                      ui->editor_ws_indent_checkbox->isChecked ());
  settings->setValue ("editor/show_eol_chars",
                      ui->cb_show_eol->isChecked ());
  settings->setValue ("editor/show_hscroll_bar",
                      ui->cb_show_hscrollbar->isChecked ());
  settings->setValue ("editor/default_eol_mode",
                      ui->combo_eol_mode->currentIndex ());
  settings->setValue ("editor/auto_indent",
                      ui->editor_auto_ind_checkbox->isChecked ());
  settings->setValue ("editor/tab_indents_line",
                      ui->editor_tab_ind_checkbox->isChecked ());
  settings->setValue ("editor/backspace_unindents_line",
                      ui->editor_bs_unind_checkbox->isChecked ());
  settings->setValue ("editor/show_indent_guides",
                      ui->editor_ind_guides_checkbox->isChecked ());
  settings->setValue ("editor/indent_width",
                      ui->editor_ind_width_spinbox->value ());
  settings->setValue ("editor/indent_uses_tabs",
                      ui->editor_ind_uses_tabs_checkbox->isChecked ());
  settings->setValue ("editor/tab_width",
                      ui->editor_tab_width_spinbox->value ());
  settings->setValue ("editor/longWindowTitle",
                      ui->editor_longWindowTitle->isChecked ());
  settings->setValue ("editor/notebook_tab_width_min",
                      ui->editor_notebook_tab_width_min->value ());
  settings->setValue ("editor/notebook_tab_width_max",
                      ui->editor_notebook_tab_width_max->value ());
  settings->setValue ("editor/restoreSession",
                      ui->editor_restoreSession->isChecked ());
  settings->setValue ("editor/create_new_file",
                      ui->editor_create_new_file->isChecked ());
  settings->setValue ("editor/always_reload_changed_files",
                      ui->editor_reload_changed_files->isChecked ());
  settings->setValue ("terminal/fontSize", ui->terminal_fontSize->value ());
  settings->setValue ("terminal/fontName",
                      ui->terminal_fontName->currentFont ().family ());

  settings->setValue ("filesdockwidget/showFileSize",
                      ui->showFileSize->isChecked ());
  settings->setValue ("filesdockwidget/showFileType",
                      ui->showFileType->isChecked ());
  settings->setValue ("filesdockwidget/showLastModified",
                      ui->showLastModified->isChecked ());
  settings->setValue ("filesdockwidget/showHiddenFiles",
                      ui->showHiddenFiles->isChecked ());
  settings->setValue ("filesdockwidget/useAlternatingRowColors",
                      ui->useAlternatingRowColors->isChecked ());
  settings->setValue ("filesdockwidget/sync_octave_directory",
                      ui->sync_octave_directory->isChecked ());
  settings->setValue ("filesdockwidget/restore_last_dir",
                      ui->cb_restore_file_browser_dir->isChecked ());
  settings->setValue ("filesdockwidget/startup_dir",
                      ui->le_file_browser_dir->text ());


  settings->setValue ("news/allow_web_connection",
                      ui->checkbox_allow_web_connect->isChecked ());
  settings->setValue ("useProxyServer", ui->useProxyServer->isChecked ());
  settings->setValue ("proxyType", ui->proxyType->currentText ());
  settings->setValue ("proxyHostName", ui->proxyHostName->text ());
  settings->setValue ("proxyPort", ui->proxyPort->text ());
  settings->setValue ("proxyUserName", ui->proxyUserName->text ());
  settings->setValue ("proxyPassword", ui->proxyPassword->text ());
  settings->setValue ("terminal/cursorBlinking",
                      ui->terminal_cursorBlinking->isChecked ());
  settings->setValue ("terminal/cursorUseForegroundColor",
                      ui->terminal_cursorUseForegroundColor->isChecked ());
  settings->setValue ("terminal/focus_after_command",
                      ui->terminal_focus_command->isChecked ());
  settings->setValue ("terminal/print_debug_location",
                      ui->terminal_print_dbg_location->isChecked ());
  settings->setValue ("terminal/history_buffer",
                      ui->terminal_history_buffer->value ());

  // the cursor
  QString cursorType;
  switch (ui->terminal_cursorType->currentIndex ())
    {
    case 0: cursorType = "ibeam"; break;
    case 1: cursorType = "block"; break;
    case 2: cursorType = "underline";  break;
    }
  settings->setValue ("terminal/cursorType", cursorType);

#ifdef HAVE_QSCINTILLA
  // editor styles: create lexer, get dialog contents, and write settings
  QsciLexer *lexer;
#if defined (HAVE_LEXER_OCTAVE)
  lexer = new QsciLexerOctave ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#elif defined (HAVE_LEXER_MATLAB)
  lexer = new QsciLexerMatlab ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#endif
  lexer = new QsciLexerCPP ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerPerl ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBatch ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerDiff ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBash ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new octave_txt_lexer ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#endif

  // Workspace
  write_workspace_colors (settings);
  // hide tool tips
  settings->setValue ("workspaceview/hide_tool_tips",
                      ui->cb_hide_tool_tips->isChecked ());

  // Terminal
  write_terminal_colors (settings);

  // shortcuts
  settings->setValue ("shortcuts/prevent_readline_conflicts",
                      ui->cb_prevent_readline_conflicts->isChecked ());
  int set = 0;
  if (ui->rb_sc_set2->isChecked ())
    set = 1;
  settings->setValue ("shortcuts/set",set);
  shortcut_manager::write_shortcuts (0, settings, closing); // 0: write both sets

  // settings dialog's geometry
  settings->setValue ("settings/last_tab",ui->tabWidget->currentIndex ());
  settings->setValue ("settings/geometry",saveGeometry ());

  settings->sync ();
}

#ifdef HAVE_QSCINTILLA
void
settings_dialog::write_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  QWidget *tab = ui->tabs_editor_lexers->
                 findChild <QWidget *>(QString (lexer->language ())+"_styles");
  int styles[MaxLexerStyles];  // array for saving valid styles
                               // (enum is not continuous)
  int max_style = get_valid_lexer_styles (lexer, styles);
  QFontComboBox *select_font;
  QSpinBox *font_size;
  QCheckBox *attrib_font[3];
  color_picker *color;
  color_picker *bg_color;
  int default_size = 10;
  QFont default_font = QFont ("Courier New",10,-1,0);
  QColor default_color = QColor ();
  QColor dummy_color = QColor (255,0,255);

  for (int i = 0; i < max_style; i++)  // get dialog elements and their contents
    {
      QString actual_name = lexer->description (styles[i]);
      select_font    = tab->findChild <QFontComboBox *>(actual_name+"_font");
      font_size      = tab->findChild <QSpinBox *>(actual_name+"_size");
      attrib_font[0] = tab->findChild <QCheckBox *>(actual_name+"_bold");
      attrib_font[1] = tab->findChild <QCheckBox *>(actual_name+"_italic");
      attrib_font[2] = tab->findChild <QCheckBox *>(actual_name+"_underline");
      color          = tab->findChild <color_picker *>(actual_name+"_color");
      bg_color       = tab->findChild <color_picker *>(actual_name+"_bg_color");
      QFont new_font = default_font;
      if (select_font)
        {
          new_font = select_font->currentFont ();
          if (styles[i] == 0)
            default_font = new_font;
          else if (select_font->currentText () == lexer->description (0))
            new_font = default_font;
        }
      if (font_size)
        {
          if (styles[i] == 0)
            {
              default_size = font_size->value ();
              new_font.setPointSize (font_size->value ());
            }
          else
            new_font.setPointSize (font_size->value ()+default_size);
        }
      if (attrib_font[0])
        new_font.setBold (attrib_font[0]->isChecked ());
      if (attrib_font[1])
        new_font.setItalic (attrib_font[1]->isChecked ());
      if (attrib_font[2])
        new_font.setUnderline (attrib_font[2]->isChecked ());
      lexer->setFont (new_font,styles[i]);
      if (styles[i] == 0)
        lexer->setDefaultFont (new_font);
      if (color)
        lexer->setColor (color->color (),styles[i]);
      if (bg_color)
        {
          if (styles[i] == 0)
            {
              default_color = bg_color->color ();
              lexer->setPaper (default_color,styles[i]);
              lexer->setDefaultPaper (default_color);
            }
          else
            {
              if (bg_color->color () == dummy_color)
                lexer->setPaper (default_color,styles[i]);
              else
                lexer->setPaper (bg_color->color (),styles[i]);
            }
        }
    }

  lexer->writeSettings (*settings);

  settings->setValue (
    "settings/last_editor_styles_tab",ui->tabs_editor_lexers->currentIndex ());
  settings->sync ();
}
#endif

void
settings_dialog::write_workspace_colors (QSettings *settings)
{

  QString class_chars = resource_manager::storage_class_chars ();
  color_picker *color;

  for (int i = 0; i < class_chars.length (); i++)
    {
      color = ui->workspace_colors_box->findChild <color_picker *>(
                "color_"+class_chars.mid (i,1));
      if (color)
        settings->setValue ("workspaceview/color_"+class_chars.mid (i,1),
                            color->color ());
    }
  settings->sync ();
}

void
settings_dialog::write_terminal_colors (QSettings *settings)
{
  QString class_chars = resource_manager::terminal_color_chars ();
  color_picker *color;

  for (int i = 0; i < class_chars.length (); i++)
    {
      color = ui->terminal_colors_box->findChild <color_picker *>(
                "terminal_color_"+class_chars.mid (i,1));
      if (color)
        settings->setValue ("terminal/color_"+class_chars.mid (i,1),
                            color->color ());
    }
  settings->sync ();
}


// internal slots

void
settings_dialog::button_clicked (QAbstractButton *button)
{
  QDialogButtonBox::ButtonRole button_role = ui->button_box->buttonRole (button);

  if (button_role == QDialogButtonBox::ApplyRole ||
      button_role == QDialogButtonBox::AcceptRole)
    {
      write_changed_settings (button_role == QDialogButtonBox::AcceptRole);
      emit apply_new_settings ();
    }

  if (button_role == QDialogButtonBox::RejectRole ||
      button_role == QDialogButtonBox::AcceptRole)
    close ();
}

void
settings_dialog::get_dir (QLineEdit *line_edit, const QString& title)
{
  QString dir = QFileDialog::getExistingDirectory(this,
                title, line_edit->text (),
                QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);
  line_edit->setText (dir);
}

void
settings_dialog::get_octave_dir ()
{
  get_dir (ui->le_octave_dir, tr ("Set Octave Startup Directory"));
}

void
settings_dialog::get_file_browser_dir ()
{
  get_dir (ui->le_file_browser_dir, tr ("Set File Browser Startup Directory"));
}

void
settings_dialog::set_disabled_pref_file_browser_dir (bool disable)
{
  ui->cb_restore_file_browser_dir->setDisabled (disable);

  if (! disable)
    {
      ui->le_file_browser_dir->setDisabled (
        ui->cb_restore_file_browser_dir->isChecked ());
      ui->pb_file_browser_dir->setDisabled (
        ui->cb_restore_file_browser_dir->isChecked ());
    }
  else
    {
      ui->le_file_browser_dir->setDisabled (disable);
      ui->pb_file_browser_dir->setDisabled (disable);
    }
}

// slots for import/export of shortcut sets
void
settings_dialog::import_shortcut_set1 ()
{
  shortcut_manager::import_export (true,1);
}

void
settings_dialog::export_shortcut_set1 ()
{
  shortcut_manager::import_export (false,1);
}

void
settings_dialog::import_shortcut_set2 ()
{
  shortcut_manager::import_export (true,2);
}

void
settings_dialog::export_shortcut_set2 ()
{
  shortcut_manager::import_export (false,2);
}

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

#if !defined (octave_file_editor_tab_h)
#define octave_file_editor_tab_h 1

#include <QWidget>
#include <QCloseEvent>
#include <QFileSystemWatcher>
#include <QSettings>
#include <QFileInfo>
#include <Qsci/qsciapis.h>
#include <QStatusBar>
#include <QLabel>
#include <QComboBox>

#include "find-dialog.h"
#include "octave-qscintilla.h"
#include "builtin-defun-decls.h"

class file_editor;

class file_editor_tab : public QWidget
{
  Q_OBJECT

public:

  file_editor_tab (const QString& directory = "");

  ~file_editor_tab (void);

  octave_qscintilla *qsci_edit_area () { return _edit_area; }

  // Will initiate close if associated with the identifier tag.
  bool conditional_close (void);

  static void reset_cancel (void) {_cancelled = false;}
  static bool was_cancelled (void) {return _cancelled;}

public slots:

  void update_window_title (bool modified);
  void handle_copy_available (bool enableCopy);
  void handle_margin_clicked (int line, int margin,
                              Qt::KeyboardModifiers state);

  // Tells the editor tab to react on changed settings.
  void notice_settings (const QSettings *settings, bool init = false);

  // Change to a different editor tab by identifier tag.
  void change_editor_state (const QWidget *ID);

  // Simply transmit file name.
  void file_name_query (const QWidget *ID);

  void set_focus (const QWidget *ID);
  void set_current_directory (const QString& dir);
  void context_help (const QWidget *ID, bool);
  void context_edit (const QWidget *ID);
  void check_modified_file (void);
  void save_file (const QWidget *ID);
  void save_file (const QWidget *ID, const QString& fileName,
                  bool remove_on_success);
  void save_file_as (const QWidget *ID);
  void print_file (const QWidget *ID);
  void run_file (const QWidget *ID);
  void context_run (const QWidget *ID);
  void toggle_bookmark (const QWidget *ID);
  void next_bookmark (const QWidget *ID);
  void previous_bookmark (const QWidget *ID);
  void remove_bookmark (const QWidget *ID);

  void toggle_breakpoint (const QWidget *ID);
  void next_breakpoint (const QWidget *ID);
  void previous_breakpoint (const QWidget *ID);
  void remove_all_breakpoints (const QWidget *ID);

  void scintilla_command (const QWidget *, unsigned int);

  void comment_selected_text (const QWidget *ID);
  void uncomment_selected_text (const QWidget *ID);

  void indent_selected_text (const QWidget *ID);
  void unindent_selected_text (const QWidget *ID);
  void convert_eol (const QWidget *ID, QsciScintilla::EolMode);

  void zoom_in (const QWidget *ID);
  void zoom_out (const QWidget *ID);
  void zoom_normal (const QWidget *ID);

  void find (const QWidget *ID);
  void goto_line (const QWidget *ID, int line = -1);
  void move_match_brace (const QWidget *ID, bool select);
  void show_auto_completion (const QWidget *ID);

  void insert_debugger_pointer (const QWidget *ID, int line = -1);
  void delete_debugger_pointer (const QWidget *ID, int line = -1);

  void do_breakpoint_marker (bool insert, const QWidget *ID, int line = -1);

  void set_modified (bool modified = true);

  QString load_file (const QString& fileName);
  void new_file (const QString& commands = QString ());

  void file_has_changed (const QString& fileName);

  void handle_context_menu_edit (const QString&);

signals:

  void file_name_changed (const QString& fileName, const QString& toolTip);
  void editor_state_changed (bool copy_available, bool is_octave_file);
  void tab_remove_request ();
  void add_filename_to_list (const QString&, QWidget *);
  void mru_add_file (const QString& file_name);
  void editor_check_conflict_save (const QString& saveFileName,
                                   bool remove_on_success);
  void run_file_signal (const QFileInfo& info);
  void set_global_edit_shortcuts_signal (bool);
  void request_open_file (const QString&);

protected:

  void closeEvent (QCloseEvent *event);
  void set_file_name (const QString& fileName);

private slots:

  // When user closes message box for reload question.
  void handle_file_reload_answer (int decision);

  // When user closes message box for resave question.
  void handle_file_resave_answer (int decision);

  // When user closes message box for modified question.
  void handle_file_modified_answer (int decision);

  // When user closes find_dialog box.
  void handle_find_dialog_finished (int decision);

  // When user closes QFileDialog box.
  void handle_save_file_as_answer (const QString& fileName);
  void handle_save_file_as_answer_close (const QString& fileName);
  void handle_save_file_as_answer_cancel ();
  void handle_save_as_filter_selected (const QString& filter);
  void handle_combo_eol_current_index (int index);

  // When apis preparation has finished and is ready to save
  void save_apis_info ();

  // When the numer of lines changes -> adapt width of margin
  void auto_margin_width ();

  void handle_cursor_moved (int line, int col);

private:

  enum editor_markers
  {
    bookmark,
    breakpoint,
    debugger_position
  };

  struct bp_info
  {
    bp_info (const QString& fname, int l = 0);

    int line;
    std::string file;
    std::string dir;
    std::string function_name;
  };

  bool valid_file_name (const QString& file=QString ());
  void save_file (const QString& saveFileName, bool remove_on_success = false);
  void save_file_as (bool remove_on_success = false);
  bool check_valid_identifier (QString file_name);

  void update_lexer ();
  void request_add_breakpoint (int line);
  void request_remove_breakpoint (int line);

  void show_dialog (QDialog *dlg, bool modal);
  int check_file_modified ();
  void do_comment_selected_text (bool comment);
  QString comment_string (const QString&);
  void do_indent_selected_text (bool indent);

  void add_breakpoint_callback (const bp_info& info);
  void remove_breakpoint_callback (const bp_info& info);
  void remove_all_breakpoints_callback (const bp_info& info);
  void center_current_line ();

  void add_octave_apis (octave_value_list key_ovl);
  QString get_function_name ();

  QsciScintilla::EolMode detect_eol_mode ();
  void update_eol_indicator ();

  octave_qscintilla *_edit_area;

  QStatusBar *_status_bar;
  QLabel *_row_indicator;
  QLabel *_col_indicator;
  QLabel *_eol_indicator;

  QsciScintilla::EolMode _save_as_desired_eol;

  QString _file_name;
  QString _file_name_short;
  QString _ced;

  bool _long_title;
  bool _copy_available;
  bool _is_octave_file;
  bool _always_reload_changed_files;

  QFileSystemWatcher _file_system_watcher;

  find_dialog *_find_dialog;
  bool _find_dialog_is_visible;
  QRect _find_dialog_geometry;

  QsciAPIs *_lexer_apis;
  QString _prep_apis_file;

  static bool _cancelled;
};

#endif

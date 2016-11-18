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

#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include <QToolBar>
#include <QAction>
#include <QMenuBar>
#include <QStatusBar>
#include <QCloseEvent>
#include <QTabWidget>
#include <QStackedWidget>

#include <QDragEnterEvent>
#include <QDropEvent>

#include <map>

#include "file-editor-interface.h"
#include "file-editor-tab.h"

class file_editor : public file_editor_interface
{
  Q_OBJECT

public:

  typedef std::map<QString, QWidget *>::iterator editor_tab_map_iterator;
  typedef std::map<QString, QWidget *>::const_iterator editor_tab_map_const_iterator;

  file_editor (QWidget *p);
  ~file_editor (void);

  void loadFile (const QString& fileName);

  QMenu *get_mru_menu (void) { return _mru_file_menu; }
  QMenu *debug_menu (void);
  QToolBar *toolbar (void);
  void insert_new_open_actions (QAction*,QAction*,QAction*);

  void handle_enter_debug_mode (void);
  void handle_exit_debug_mode (void);

  void check_actions (void);
  void empty_script (bool startup, bool visible);

signals:

  void fetab_settings_changed (const QSettings *settings);
  void fetab_change_request (const QWidget* ID);
  void fetab_file_name_query (const QWidget* ID);
  // Save is a ping-pong type of communication
  void fetab_save_file (const QWidget* ID, const QString& fileName,
                        bool remove_on_success);
  // No fetab_open, functionality in editor
  // No fetab_new, functionality in editor
  void fetab_context_help (const QWidget* ID, bool);
  void fetab_context_edit (const QWidget* ID);
  void fetab_check_modified_file (void);
  void fetab_save_file (const QWidget* ID);
  void fetab_save_file_as (const QWidget* ID);
  void fetab_print_file (const QWidget* ID);
  void fetab_run_file (const QWidget* ID);
  void fetab_context_run (const QWidget* ID);
  void fetab_toggle_bookmark (const QWidget* ID);
  void fetab_next_bookmark (const QWidget* ID);
  void fetab_previous_bookmark (const QWidget* ID);
  void fetab_remove_bookmark (const QWidget* ID);
  void fetab_toggle_breakpoint (const QWidget* ID);
  void fetab_next_breakpoint (const QWidget* ID);
  void fetab_previous_breakpoint (const QWidget* ID);
  void fetab_remove_all_breakpoints (const QWidget* ID);
  void fetab_comment_selected_text (const QWidget* ID);
  void fetab_uncomment_selected_text (const QWidget* ID);
  void fetab_indent_selected_text (const QWidget* ID);
  void fetab_unindent_selected_text (const QWidget* ID);
  void fetab_convert_eol (const QWidget* ID, QsciScintilla::EolMode eol_mode);
  void fetab_find (const QWidget* ID);
  void fetab_goto_line (const QWidget* ID, int line = -1);
  void fetab_move_match_brace (const QWidget* ID, bool select);
  void fetab_completion (const QWidget*);
  void fetab_insert_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_delete_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_do_breakpoint_marker (bool insert, const QWidget* ID,
                                   int line = -1);
  void fetab_set_focus (const QWidget* ID);
  void fetab_scintilla_command (const QWidget* ID, unsigned int sci_msg);

  void fetab_zoom_in (const QWidget* ID);
  void fetab_zoom_out (const QWidget* ID);
  void fetab_zoom_normal (const QWidget* ID);

  void fetab_set_directory (const QString& dir);

  void request_settings_dialog (const QString&);
  void execute_command_in_terminal_signal (const QString&);
  void file_loaded_signal ();

public slots:

  void focus (void);
  void enable_menu_shortcuts (bool);
  bool check_closing (void);

  void request_new_file (const QString& commands);
  void request_new_script (const QString& commands);
  void request_new_function (bool triggered = true);
  void request_open_file (void);
  void request_close_file (bool);
  void request_close_all_files (bool);
  void request_close_other_files (bool);
  void request_mru_open_file (QAction *action);
  void request_print_file (bool);

  void request_undo (bool);
  void request_redo (bool);
  void request_copy (bool);
  void request_cut (bool);
  void request_paste (bool);
  void request_selectall (bool);
  void request_context_help (bool);
  void request_context_doc (bool);
  void request_context_edit (bool);
  void request_save_file (bool);
  void request_save_file_as (bool);
  void request_run_file (bool);
  void request_context_run (bool);
  void request_toggle_bookmark (bool);
  void request_next_bookmark (bool);
  void request_previous_bookmark (bool);
  void request_remove_bookmark (bool);

  void request_move_match_brace (bool);
  void request_sel_match_brace (bool);
  void request_toggle_breakpoint (bool);
  void request_next_breakpoint (bool);
  void request_previous_breakpoint (bool);
  void request_remove_breakpoint (bool);

  void request_delete_start_word (bool);
  void request_delete_end_word (bool);
  void request_delete_start_line (bool);
  void request_delete_end_line (bool);
  void request_delete_line (bool);
  void request_copy_line (bool);
  void request_cut_line (bool);
  void request_duplicate_selection (bool);
  void request_transpose_line (bool);

  void request_comment_selected_text (bool);
  void request_uncomment_selected_text (bool);

  void request_upper_case (bool);
  void request_lower_case (bool);
  void request_indent_selected_text (bool);
  void request_unindent_selected_text (bool);
  void request_conv_eol_windows (bool);
  void request_conv_eol_unix (bool);
  void request_conv_eol_mac (bool);

  void request_find (bool);

  void request_goto_line (bool);
  void request_completion (bool);

  void handle_file_name_changed (const QString& fileName,
                                 const QString& toolTip);
  void handle_tab_close_request (int index);
  void handle_tab_remove_request (void);
  void handle_add_filename_to_list (const QString& fileName, QWidget *ID);
  void active_tab_changed (int index);
  void handle_editor_state_changed (bool enableCopy, bool is_octave_file);
  void handle_mru_add_file (const QString& file_name);
  void check_conflict_save (const QString& fileName, bool remove_on_success);

  void handle_insert_debugger_pointer_request (const QString& file, int line);
  void handle_delete_debugger_pointer_request (const QString& file, int line);
  void handle_update_breakpoint_marker_request (bool insert,
                                                const QString& file, int line);

  void handle_edit_file_request (const QString& file);

  // Tells the editor to react on changed settings.
  void notice_settings (const QSettings *settings);

  void set_shortcuts ();

  void handle_visibility (bool visible);

  void update_octave_directory (const QString& dir);

protected slots:
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();
  void do_undo ();

private slots:

  void request_open_files (const QStringList&);
  void request_open_file (const QString& fileName, int line = -1,
                          bool debug_pointer = false,
                          bool breakpoint_marker = false, bool insert = true);
  void request_preferences (bool);
  void request_styles_preferences (bool);
  void restore_create_file_setting ();

  void show_line_numbers (bool);
  void show_white_space (bool);
  void show_eol_chars (bool);
  void show_indent_guides (bool);
  void show_long_line (bool);
  void zoom_in (bool);
  void zoom_out (bool);
  void zoom_normal (bool);

  void create_context_menu (QMenu *);
  void edit_status_update (bool, bool);

protected:

  void dragEnterEvent(QDragEnterEvent *event);
  void dropEvent(QDropEvent *event);

private:

  bool is_editor_console_tabbed ();
  void construct (void);
  void add_file_editor_tab (file_editor_tab *f, const QString& fn);
  void save_file_as (QWidget *fetabID = 0);
  void mru_menu_update (void);
  bool call_custom_editor (const QString& file_name = QString (), int line = -1);

  void toggle_preference (const QString& preference, bool def);

  bool editor_tab_has_focus ();

  QWidget *find_tab_widget (const QString& openFileName) const;
  QAction *add_action (QMenu *menu, const QIcon &icon, const QString &text,
                       const char *member);

  QMenu* m_add_menu (QMenuBar *p, QString text);

  std::map<QString, QWidget *> editor_tab_map;
  QHash<QMenu*, QStringList> _hash_menu_text;

  QString ced;

  QMenuBar *_menu_bar;
  QToolBar *_tool_bar;
  QMenu *_debug_menu;

  QAction *_new_action;
  QAction *_new_function_action;
  QAction *_open_action;

  QAction *_upper_case_action;
  QAction *_lower_case_action;
  QAction *_comment_selection_action;
  QAction *_uncomment_selection_action;
  QAction *_indent_selection_action;
  QAction *_unindent_selection_action;
  QAction *_conv_eol_windows_action;
  QAction *_conv_eol_unix_action;
  QAction *_conv_eol_mac_action;

  QAction *_copy_action;
  QAction *_cut_action;
  QAction *_paste_action;
  QAction *_selectall_action;
  QAction *_context_help_action;
  QAction *_context_doc_action;

  QAction *_show_linenum_action;
  QAction *_show_whitespace_action;
  QAction *_show_eol_action;
  QAction *_show_indguide_action;
  QAction *_show_longline_action;
  QAction *_zoom_in_action;
  QAction *_zoom_out_action;
  QAction *_zoom_normal_action;

  QAction *_delete_start_word_action;
  QAction *_delete_end_word_action;
  QAction *_delete_start_line_action;
  QAction *_delete_end_line_action;
  QAction *_delete_line_action;
  QAction *_copy_line_action;
  QAction *_cut_line_action;
  QAction *_duplicate_selection_action;
  QAction *_transpose_line_action;

  QAction *_find_action;
  QAction *_goto_line_action;
  QAction *_completion_action;

  QAction *_move_to_matching_brace;
  QAction *_sel_to_matching_brace;
  QAction *_next_bookmark_action;
  QAction *_previous_bookmark_action;
  QAction *_toggle_bookmark_action;
  QAction * _remove_bookmark_action;

  QAction *_print_action;
  QAction *_run_action;
  QAction *_run_selection_action;

  QAction *_edit_function_action;
  QAction *_save_action;
  QAction *_save_as_action;
  QAction *_close_action;
  QAction *_close_all_action;
  QAction *_close_others_action;

  QAction *_redo_action;
  QAction *_undo_action;

  QAction *_preferences_action;
  QAction *_styles_preferences_action;

  QAction *_toggle_breakpoint_action;
  QAction *_next_breakpoint_action;
  QAction *_previous_breakpoint_action;
  QAction *_remove_all_breakpoints_action;

  QMenu *_edit_cmd_menu;
  QMenu *_edit_fmt_menu;
  QMenu *_edit_nav_menu;
  QMenu *_fileMenu;
  QMenu *_view_editor_menu;

  QTabWidget *_tab_widget;

  int _marker_breakpoint;

  enum { MaxMRUFiles = 10 };
  QMenu *_mru_file_menu;
  QAction *_mru_file_actions[MaxMRUFiles];
  QStringList _mru_files;
};

#endif // FILEEDITORMDISUBWINDOW_H

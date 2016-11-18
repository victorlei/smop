/*

Copyright (C) 2013-2015 John W. Eaton
Copyright (C) 2011-2015 Jacob Dawid
Copyright (C) 2011-2015 John P. Swensen

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

#ifndef OCTAVE_QT_LINK_H
#define OCTAVE_QT_LINK_H

#include <list>
#include <string>

#include <QList>
#include <QObject>
#include <QString>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>

#include "octave-link.h"
#include "octave-interpreter.h"

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

// @class OctaveLink
// @brief Provides threadsafe access to octave.
// @author Jacob Dawid
//
// This class is a wrapper around octave and provides thread safety by
// buffering access operations to octave and executing them in the
// readline event hook, which lives in the octave thread.

class octave_qt_link : public QObject, public octave_link
{
  Q_OBJECT

public:

  octave_qt_link (QWidget *p);

  ~octave_qt_link (void);

  void execute_interpreter (void);

  bool do_confirm_shutdown (void);
  bool do_exit (int status);

  bool do_copy_image_to_clipboard (const std::string& file);

  bool do_edit_file (const std::string& file);
  bool do_prompt_new_edit_file (const std::string& file);

  int do_message_dialog (const std::string& dlg, const std::string& msg,
                         const std::string& title);

  std::string
  do_question_dialog (const std::string& msg, const std::string& title,
                      const std::string& btn1, const std::string& btn2,
                      const std::string& btn3, const std::string& btndef);

  std::pair<std::list<int>, int>
  do_list_dialog (const std::list<std::string>& list,
                  const std::string& mode,
                  int width, int height,
                  const std::list<int>& initial_value,
                  const std::string& name,
                  const std::list<std::string>& prompt,
                  const std::string& ok_string,
                  const std::string& cancel_string);

  std::list<std::string>
  do_input_dialog (const std::list<std::string>& prompt,
                   const std::string& title,
                   const std::list<float>& nr,
                   const std::list<float>& nc,
                   const std::list<std::string>& defaults);

  std::list<std::string>
  do_file_dialog (const filter_list& filter, const std::string& title,
                  const std::string &filename, const std::string &pathname,
                  const std::string& multimode);

  int
  do_debug_cd_or_addpath_error (const std::string& file,
                                const std::string& dir,
                                bool addpath_option);

  void do_change_directory (const std::string& dir);

  void do_execute_command_in_terminal (const std::string& command);

  void do_set_workspace (bool top_level, bool debug,
                         const std::list<workspace_element>& ws);

  void do_clear_workspace (void);

  void do_set_history (const string_vector& hist);
  void do_append_history (const std::string& hist_entry);
  void do_clear_history (void);

  void do_pre_input_event (void);
  void do_post_input_event (void);

  void do_enter_debugger_event (const std::string& file, int line);
  void do_execute_in_debugger_event (const std::string& file, int line);
  void do_exit_debugger_event (void);

  void do_update_breakpoint (bool insert, const std::string& file, int line);

  void do_set_default_prompts (std::string& ps1, std::string& ps2,
                               std::string& ps4);

  static bool file_in_path (const std::string& file, const std::string& dir);

  void do_show_preferences (void);

  void do_show_doc (const std::string& file);

  QMutex mutex;
  QWaitCondition waitcondition;
  void shutdown_confirmation (bool sd) {_shutdown_confirm_result = sd;}

  void update_directory (void);

private:

  bool _shutdown_confirm_result;

  octave_qt_link (const octave_qt_link&);

  octave_qt_link& operator = (const octave_qt_link&);

  void do_insert_debugger_pointer (const std::string& file, int line);
  void do_delete_debugger_pointer (const std::string& file, int line);

  // Thread running octave_main.
  QThread *main_thread;

  octave_interpreter *command_interpreter;

  QString _current_directory;
  bool    _new_dir;

signals:

  void execute_interpreter_signal (void);

  void copy_image_to_clipboard_signal (const QString& file, bool remove_file);

  void edit_file_signal (const QString& file);

  void change_directory_signal (const QString& dir);

  void execute_command_in_terminal_signal (const QString& command);

  void set_workspace_signal (bool top_level,
                             bool debug,
                             const QString& scopes,
                             const QStringList& symbols,
                             const QStringList& class_names,
                             const QStringList& dimensions,
                             const QStringList& values,
                             const QIntList& complex_flags);

  void clear_workspace_signal (void);

  void set_history_signal (const QStringList& hist);
  void append_history_signal (const QString& hist_entry);
  void clear_history_signal (void);

  void enter_debugger_signal (void);
  void exit_debugger_signal (void);

  void update_breakpoint_marker_signal (bool insert, const QString& file,
                                        int line);

  void insert_debugger_pointer_signal (const QString&, int);
  void delete_debugger_pointer_signal (const QString&, int);

  void show_preferences_signal (void);

  void show_doc_signal (const QString &file);

  void confirm_shutdown_signal (void);
  void exit_app_signal (int status);

public slots:

  void terminal_interrupt (void);
};

#endif

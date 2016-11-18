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

#ifndef FILEEDITORINTERFACE_H
#define FILEEDITORINTERFACE_H

#include <QMenu>
#include <QToolBar>
#include "octave-dock-widget.h"

class file_editor_interface : public octave_dock_widget
{
  Q_OBJECT

public:
  file_editor_interface (QWidget *p)
    : octave_dock_widget (p)
  {
    setObjectName ("FileEditor");
  }

  virtual ~file_editor_interface () { }

  virtual QMenu *get_mru_menu () = 0;
  virtual QMenu *debug_menu () = 0;
  virtual QToolBar *toolbar () = 0;

  virtual void insert_new_open_actions (QAction*,QAction*,QAction*) = 0;
  virtual void handle_enter_debug_mode (void) = 0;
  virtual void handle_exit_debug_mode (void) = 0;

  virtual void
  handle_insert_debugger_pointer_request (const QString& file, int line) = 0;

  virtual void
  handle_delete_debugger_pointer_request (const QString& file, int line) = 0;

  virtual void
  handle_update_breakpoint_marker_request (bool insert, const QString& file,
                                           int line) = 0;

  virtual void handle_edit_file_request (const QString& file) = 0;

  virtual bool check_closing (void) = 0;

  virtual void empty_script (bool, bool) = 0;

  virtual void enable_menu_shortcuts (bool enable) = 0;

public slots:
  virtual void request_new_file (const QString& command = QString ()) = 0;
  virtual void request_new_script (const QString& command = QString ()) = 0;
  virtual void request_new_function (bool) = 0;
  virtual void request_open_file () = 0;
  virtual void request_open_file (const QString& openFileName, int line = -1,
                                  bool debug_pointer = false,
                                  bool breakpoint_marker = false,
                                  bool insert = true) = 0;
//signals:

//protected:

//protected slots:

};

#endif // FILEEDITORINTERFACE_H

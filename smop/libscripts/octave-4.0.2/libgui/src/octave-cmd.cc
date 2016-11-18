/*

Copyright (C) 2014 Torsten

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

// Author: Torsten <ttl@justmail.de>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "octave-cmd.h"

#include "octave-qt-link.h"
#include "cmd-edit.h"
#include "builtin-defun-decls.h"
#include "utils.h"


// ---------------------------------------------------------------------
//  class octave_cmd_exec: executing a command

void
octave_cmd_exec::execute ()
{
  std::string pending_input = command_editor::get_current_line ();

  command_editor::set_initial_input (pending_input);
  command_editor::replace_line (_cmd.toStdString ());
  command_editor::redisplay ();
  command_editor::accept_line ();
}


// ---------------------------------------------------------------------
//  class octave_cmd_eval: running a file

void
octave_cmd_eval::execute ()
{
  QString function_name = _info.fileName ();
  function_name.chop (_info.suffix ().length () + 1);
  std::string file_path = _info.absoluteFilePath ().toStdString ();

  std::string pending_input = command_editor::get_current_line ();

  if (valid_identifier (function_name.toStdString ()))
    {
      // valid identifier: call as function with possibility to debug
      std::string path = _info.absolutePath ().toStdString ();
      if (octave_qt_link::file_in_path (file_path, path))
        command_editor::replace_line (function_name.toStdString ());
    }
  else
    {
      // no valid identifier: use Fsource (), no debug possible
      Fsource (ovl (file_path));
      command_editor::replace_line ("");
    }

  command_editor::set_initial_input (pending_input);
  command_editor::redisplay ();

  command_editor::accept_line ();
}

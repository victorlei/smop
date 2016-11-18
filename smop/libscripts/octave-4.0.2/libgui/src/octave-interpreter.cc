/*

Copyright (C) 2013-2015 John W. Eaton
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

#include <string>

#include <signal.h>

#include "octave.h"

#include "octave-interpreter.h"

void
octave_interpreter::execute (void)
{
  thread_manager.register_current_thread ();

  octave_thread_manager::unblock_interrupt_signal ();

  octave_initialize_interpreter (octave_cmdline_argc, octave_cmdline_argv,
                                 octave_embedded);

  emit octave_ready_signal ();

  octave_execute_interpreter ();
}

void
octave_interpreter::interrupt (void)
{
  thread_manager.interrupt ();
}

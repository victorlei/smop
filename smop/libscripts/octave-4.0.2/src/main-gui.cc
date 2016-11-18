/*

Copyright (C) 2012-2015 John W. Eaton

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

#include "defaults.h"
#include "octave.h"
#include "octave-gui.h"
#include "sysdep.h"

int
main (int argc, char **argv)
{
  octave_process_command_line (argc, argv);

  sysdep_init ();

  install_defaults ();

  bool start_gui = octave_starting_gui ();

  return octave_start_gui (argc, argv, start_gui);
}

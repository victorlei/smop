/*

Copyright (C) 2009-2015 John W. Eaton

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

#include "singleton-cleanup.h"

#include "cdisplay.h"
#include "display.h"
#include "error.h"

display_info *display_info::instance = 0;

void
display_info::init (bool query)
{
  if (query)
    {
      int avail = 0;

      const char *msg = octave_get_display_info (&ht, &wd, &dp, &rx, &ry,
                                                 &avail);

      dpy_avail = avail;

      if (msg)
        err_msg = msg;
    }
}

bool
display_info::instance_ok (bool query)
{
  bool retval = true;

  if (! instance)
    {
      instance = new display_info (query);

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create display_info object!");

      retval = false;
    }

  return retval;
}

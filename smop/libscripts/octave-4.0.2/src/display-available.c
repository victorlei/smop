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

#include <stdlib.h>

#if defined (OCTAVE_USE_WINDOWS_API)
#include <windows.h>
#elif defined (HAVE_FRAMEWORK_CARBON)
#include <Carbon/Carbon.h>
#elif defined (HAVE_X_WINDOWS)
#include <X11/Xlib.h>
#endif

#include "display-available.h"

const char *
display_available (int *dpy_avail)
{
  *dpy_avail = 0;

  const char *err_msg = "";

#if defined (OCTAVE_USE_WINDOWS_API)

  HDC hdc = GetDC (0);

  if (hdc)
    *dpy_avail = 1;
  else
    err_msg = "no graphical display found";

#elif defined (HAVE_FRAMEWORK_CARBON)

  CGDirectDisplayID display = CGMainDisplayID ();

  if (display)
    *dpy_avail = 1;
  else
    err_msg = "no graphical display found";

#elif defined (HAVE_X_WINDOWS)

  const char *display_name = getenv ("DISPLAY");

  if (display_name && *display_name)
    {
      Display *display = XOpenDisplay (display_name);

      if (display)
        {
          Screen *screen = DefaultScreenOfDisplay (display);

          if (! screen)
            err_msg = "X11 display has no default screen";

          XCloseDisplay (display);

          *dpy_avail = 1;
        }
      else
        err_msg = "unable to open X11 DISPLAY";
    }
  else
    err_msg = "X11 DISPLAY environment variable not set";

#else

  err_msg = "no graphical display found";

#endif

  return err_msg;
}

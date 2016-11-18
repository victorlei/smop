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

#if !defined (octave_display_h)
#define octave_display_h 1

#include <string>

class Matrix;

class
OCTINTERP_API
display_info
{
protected:

  display_info (bool query = true)
    : ht (1), wd (1), dp (0), rx (72), ry (72), dpy_avail (false),
      err_msg ()
  {
    init (query);
  }

public:

  static int height (void)
  {
    return instance_ok () ? instance->do_height () : 0;
  }

  static int width (void)
  {
    return instance_ok () ? instance->do_width () : 0;
  }

  static int depth (void)
  {
    return instance_ok () ? instance->do_depth () : 0;
  }

  static double x_dpi (void)
  {
    return instance_ok () ? instance->do_x_dpi () : 0;
  }

  static double y_dpi (void)
  {
    return instance_ok () ? instance->do_y_dpi () : 0;
  }

  static bool display_available (void)
  {
    std::string msg;
    return instance_ok () ? instance->do_display_available (msg) : false;
  }

  static bool display_available (std::string& msg)
  {
    return instance_ok () ? instance->do_display_available (msg) : false;
  }

  // To disable querying the window system for defaults, this function
  // must be called before any other display_info function.
  static void no_window_system (void)
  {
    instance_ok (false);
  }

private:

  static display_info *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // Height, width, and depth of the display.
  int ht;
  int wd;
  int dp;

  // X- and Y- Resolution of the display in dots (pixels) per inch.
  double rx;
  double ry;

  bool dpy_avail;

  std::string err_msg;

  int do_height (void) const { return ht; }
  int do_width (void) const { return wd; }
  int do_depth (void) const { return dp; }

  double do_x_dpi (void) const { return rx; }
  double do_y_dpi (void) const { return ry; }

  bool do_display_available (std::string& msg) const
  {
    msg = err_msg;

    return dpy_avail;
  }

  void init (bool query = true);

  static bool instance_ok (bool query = true);
};

#endif

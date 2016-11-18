/*

Copyright (C) 2010-2015 Kai Habel

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

#ifdef HAVE_FLTK

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#endif

#include <FL/Fl.H>
#include <FL/Fl_File_Chooser.H>

// FLTK headers may include X11/X.h which defines Complex, and that
// conflicts with Octave's Complex typedef.  We don't need the X11
// Complex definition in this file, so remove it before including Octave
// headers which may require Octave's Complex typedef.
#undef Complex

#endif

#include "defun-dld.h"
#include "file-ops.h"

DEFUN_DLD (__fltk_uigetfile__, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __fltk_uigetfile__ (@dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
#ifdef HAVE_FLTK
  // Expected argument list:
  //
  //   args(0) ... FileFilter in fltk format
  //   args(1) ... Title
  //   args(2) ... Default Filename
  //   args(3) ... PostionValue [x,y]
  //   args(4) ... SelectValue "on"/"off"/"dir"/"create"

  octave_value_list retval (3, octave_value (0));

  std::string file_filter = args(0).string_value ();
  std::string title = args(1).string_value ();
  std::string default_name = args(2).string_value ();
  Matrix pos = args(3).matrix_value ();

  int multi_type = Fl_File_Chooser::SINGLE;
  std::string flabel = "Filename:";

  std::string multi = args(4).string_value ();
  if (multi == "on")
    multi_type = Fl_File_Chooser::MULTI;
  else if (multi == "dir")
    {
      multi_type = Fl_File_Chooser::DIRECTORY;
      flabel = "Directory:";
    }
  else if (multi == "create")
    multi_type = Fl_File_Chooser::CREATE;

  Fl_File_Chooser::filename_label = flabel.c_str ();

  Fl_File_Chooser fc (default_name.c_str (), file_filter.c_str (),
                      multi_type, title.c_str ());

  fc.preview (0);

  if (multi_type == Fl_File_Chooser::CREATE)
    fc.ok_label ("Save");

  fc.show ();

  while (fc.shown ())
    Fl::wait ();

  if (fc.value ())
    {
      int file_count = fc.count ();
      std::string fname;

      //fltk uses forward slash even for windows
      std::string sep = "/";
      size_t idx;

      if (file_count == 1 && multi_type != Fl_File_Chooser::DIRECTORY)
        {
          fname = fc.value ();
          idx = fname.find_last_of (sep);
          retval(0) = fname.substr (idx + 1);
        }
      else
        {
          Cell file_cell = Cell (file_count, 1);
          for (octave_idx_type n = 1; n <= file_count; n++)
            {
              fname = fc.value (n);
              idx = fname.find_last_of (sep);
              file_cell(n - 1) = fname.substr (idx + 1);
            }
          retval(0) = file_cell;
        }

      if (multi_type == Fl_File_Chooser::DIRECTORY)
        retval(0) = file_ops::native_separator_path (std::string (fc.value ()));
      else
        {
          retval(1) = file_ops::native_separator_path (
                        std::string (fc.directory ()) + sep);
          retval(2) = fc.filter_value () + 1;
        }
    }

  fc.hide ();
  Fl::flush ();

  return retval;
#else
  error ("__fltk_uigetfile__: not available without OpenGL and FLTK libraries");
  return octave_value ();
#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/


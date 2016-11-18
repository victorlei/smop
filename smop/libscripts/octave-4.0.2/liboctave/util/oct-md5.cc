/*

Copyright (C) 2007-2015 David Bateman

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

#include <cstdio>

#include <string>
#include <vector>

#include "lo-error.h"
#include "oct-md5.h"
#include "md5.h"

static std::string
oct_md5_result_to_str (const unsigned char *buf)
{
  char tmp[33];

  sprintf (tmp,
           "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
           buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
           buf[8],  buf[9], buf[10], buf[11], buf[12], buf[13], buf[14],
           buf[15]);

  return std::string (tmp, 32);
}

std::string
oct_md5 (const std::string str)
{
  unsigned char buf[16];

  md5_buffer (str.data (), str.length (), buf);

  return oct_md5_result_to_str (buf);
}

std::string
oct_md5_file (const std::string file)
{
  std::string retval;

  FILE *ifile = gnulib::fopen (file.c_str (), "rb");

  if (ifile)
    {
      unsigned char buf[16];

      int errflag = md5_stream (ifile, buf);

      gnulib::fclose (ifile);

      if (! errflag)
        retval = oct_md5_result_to_str (buf);
      else
        (*current_liboctave_error_handler) ("internal error in md5_stream");
    }
  else
    (*current_liboctave_error_handler) ("unable to open file '%s' for reading",
                                        file.c_str ());

  return retval;
}

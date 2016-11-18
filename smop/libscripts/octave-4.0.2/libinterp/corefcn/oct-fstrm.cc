/*

Copyright (C) 1996-2015 John W. Eaton

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

#include <cerrno>
#include <cstring>

#include "error.h"
#include "oct-fstrm.h"

octave_stream
octave_fstream::create (const std::string& nm_arg, std::ios::openmode arg_md,
                        oct_mach_info::float_format ff)
{
  return octave_stream (new octave_fstream (nm_arg, arg_md, ff));
}

octave_fstream::octave_fstream (const std::string& nm_arg,
                                std::ios::openmode arg_md,
                                oct_mach_info::float_format ff)
  : octave_base_stream (arg_md, ff), nm (nm_arg)
{

#if CXX_ISO_COMPLIANT_LIBRARY

  fs.open (nm.c_str (), arg_md);

#else
  // Override default protection of 0664 so that umask will appear to
  // do the right thing.

  fs.open (nm.c_str (), arg_md, 0666);

#endif

  if (! fs)
    error (gnulib::strerror (errno));
}

// Position a stream at OFFSET relative to ORIGIN.

int
octave_fstream::seek (off_t, int)
{
  error ("fseek: invalid_operation");
  return -1;
}

// Return current stream position.

off_t
octave_fstream::tell (void)
{
  error ("ftell: invalid_operation");
  return -1;
}

// Return nonzero if EOF has been reached on this stream.

bool
octave_fstream::eof (void) const
{
  return fs.eof ();
}

void
octave_fstream::do_close (void)
{
  fs.close ();
}

std::istream *
octave_fstream::input_stream (void)
{
  std::istream *retval = 0;

  if (mode () & std::ios::in)
    retval = &fs;

  return retval;
}

std::ostream *
octave_fstream::output_stream (void)
{
  std::ostream *retval = 0;

  if (mode () & std::ios::out)
    retval = &fs;

  return retval;
}

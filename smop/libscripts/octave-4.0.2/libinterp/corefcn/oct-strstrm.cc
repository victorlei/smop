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

#include "oct-strstrm.h"

// Position a stream at OFFSET relative to ORIGIN.

int
octave_base_strstream::seek (off_t, int)
{
  error ("fseek: invalid operation");
  return -1;
}

// Return current stream position.

off_t
octave_base_strstream::tell (void)
{
  error ("ftell: invalid operation");
  return -1;
}

octave_stream
octave_istrstream::create (const char *data, std::ios::openmode arg_md,
                           oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_istrstream (data, arg_md, flt_fmt));
}

octave_stream
octave_istrstream::create (const std::string& data, std::ios::openmode arg_md,
                           oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_istrstream (data, arg_md, flt_fmt));
}

octave_stream
octave_ostrstream::create (std::ios::openmode arg_md,
                           oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_ostrstream (arg_md, flt_fmt));
}

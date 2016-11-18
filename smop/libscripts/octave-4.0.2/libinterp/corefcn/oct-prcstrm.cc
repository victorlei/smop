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

#include <cstdio>

#include "oct-prcstrm.h"
#include "sysdep.h"

octave_stream
octave_iprocstream::create (const std::string& n, std::ios::openmode arg_md,
                            oct_mach_info::float_format ff)
{
  return octave_stream (new octave_iprocstream (n, arg_md, ff));
}

octave_iprocstream::octave_iprocstream (const std::string& n,
                                        std::ios::openmode arg_md,
                                        oct_mach_info::float_format ff)
  : octave_stdiostream (n, octave_popen (n.c_str (), "r"),
                        arg_md, ff, octave_pclose)
{
}

octave_iprocstream::~octave_iprocstream (void)
{
  do_close ();
}

octave_stream
octave_oprocstream::create (const std::string& n, std::ios::openmode arg_md,
                            oct_mach_info::float_format ff)
{
  return octave_stream (new octave_oprocstream (n, arg_md, ff));
}

octave_oprocstream::octave_oprocstream (const std::string& n,
                                        std::ios::openmode arg_md,
                                        oct_mach_info::float_format ff)
  : octave_stdiostream (n, octave_popen (n.c_str (), "w"),
                        arg_md, ff, octave_pclose)
{
}

octave_oprocstream::~octave_oprocstream (void)
{
  do_close ();
}

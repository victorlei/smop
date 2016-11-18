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

#if !defined (octave_oct_prcstrm_h)
#define octave_oct_prcstrm_h 1

#include "oct-stdstrm.h"

// FIXME: why don't these classes use iprocstream and oprocstream,
//        which in turn use the octave_procbuf class?

class
octave_iprocstream : public octave_stdiostream
{
public:

  octave_iprocstream (const std::string& n,
                      std::ios::openmode arg_md = std::ios::in,
                      oct_mach_info::float_format flt_fmt
                        = oct_mach_info::native_float_format ());

  static octave_stream
  create (const std::string& n, std::ios::openmode arg_md = std::ios::in,
          oct_mach_info::float_format flt_fmt
            = oct_mach_info::native_float_format ());

protected:

  ~octave_iprocstream (void);

private:

  // No copying!

  octave_iprocstream (const octave_iprocstream&);

  octave_iprocstream& operator = (const octave_iprocstream&);
};

class
octave_oprocstream : public octave_stdiostream
{
public:

  octave_oprocstream (const std::string& n,
                      std::ios::openmode arg_md = std::ios::out,
                      oct_mach_info::float_format flt_fmt
                        = oct_mach_info::native_float_format ());

  static octave_stream
  create (const std::string& n, std::ios::openmode arg_md = std::ios::out,
          oct_mach_info::float_format flt_fmt
            = oct_mach_info::native_float_format ());

protected:

  ~octave_oprocstream (void);

private:

  // No copying!

  octave_oprocstream (const octave_oprocstream&);

  octave_oprocstream& operator = (const octave_oprocstream&);
};

#endif

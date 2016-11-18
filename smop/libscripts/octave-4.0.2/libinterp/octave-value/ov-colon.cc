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

#include <iostream>

#include "error.h"
#include "pr-output.h"
#include "oct-obj.h"
#include "ov-colon.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_magic_colon,
                                     "magic-colon", "magic-colon");

void
octave_magic_colon::print (std::ostream& os, bool)
{
  indent (os);
  print_raw (os);
}

void
octave_magic_colon::print_raw (std::ostream& os, bool) const
{
  os << ":";
}

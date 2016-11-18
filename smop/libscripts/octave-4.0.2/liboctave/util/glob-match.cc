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

#include <fnmatch.h>

#include "glob-match.h"
#include "oct-glob.h"

bool
glob_match::match (const std::string& str) const
{
  return octave_fnmatch (pat, str, fnmatch_flags);
}

string_vector
glob_match::glob (void) const
{
  return octave_glob (pat);
}

int
glob_match::opts_to_fnmatch_flags (unsigned int xopts) const
{
  int retval = 0;

  if (xopts & pathname)
    retval |= FNM_PATHNAME;

  if (xopts & noescape)
    retval |= FNM_NOESCAPE;

  if (xopts & period)
    retval |= FNM_PERIOD;

  return retval;
}

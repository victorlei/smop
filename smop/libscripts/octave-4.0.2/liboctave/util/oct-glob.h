/*

Copyright (C) 2010-2015 John W. Eaton

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

#if !defined (octave_oct_glob_h)
#define octave_oct_glob_h 1

#include "str-vec.h"

extern bool
octave_fnmatch (const string_vector& pat, const std::string& str,
                int fnmatch_flags);

extern string_vector
octave_glob (const string_vector&);

#endif

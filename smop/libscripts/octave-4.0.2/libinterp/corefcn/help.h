/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_help_h)
#define octave_help_h 1

#include <iosfwd>
#include <string>

class string_vector;

extern string_vector make_name_list (void);

extern OCTINTERP_API std::string raw_help (const std::string&, bool&);

extern OCTINTERP_API void install_built_in_docstrings (void);

// Name of the doc cache file specified on the command line.
// (--doc-cache-file file)
extern OCTINTERP_API std::string Vdoc_cache_file;

// Name of the file containing local Texinfo macros that are prepended
// to doc strings before processing.
// (--texi-macros-file)
extern OCTINTERP_API std::string Vtexi_macros_file;

// Name of the info file specified on command line.
// (--info-file file)
extern OCTINTERP_API std::string Vinfo_file;

// Name of the info reader we'd like to use.
// (--info-program program)
extern OCTINTERP_API std::string Vinfo_program;

extern OCTINTERP_API std::string do_which (const std::string& name);

#endif

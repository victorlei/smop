// %NO_EDIT_WARNING%
/*

Copyright (C) 1992-2015 John W. Eaton

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

#if !defined (octave_version_h)
#define octave_version_h 1

#define OCTAVE_VERSION %OCTAVE_VERSION%

#define OCTAVE_MAJOR_VERSION %OCTAVE_MAJOR_VERSION%

#define OCTAVE_MINOR_VERSION %OCTAVE_MINOR_VERSION%

#define OCTAVE_PATCH_VERSION %OCTAVE_PATCH_VERSION%

// The "API version" is used as a way of checking that interfaces in the
// liboctave and libinterp libraries haven't changed in a backwardly
// incompatible way when loading .oct files.  A better way to do that is
// with library versioning, but not all systems support that.
// NOTE: This macro will be removed in a future version of Octave.  If
// you insist on checking for features using a version number, use the
// OCTAVE_MAJOR_VERSION, OCTAVE_MINOR_VERSION, and
// OCTAVE_PATCH_VERSION macros instead.
#define OCTAVE_API_VERSION %OCTAVE_API_VERSION%

#define OCTAVE_RELEASE_DATE %OCTAVE_RELEASE_DATE%

#define OCTAVE_COPYRIGHT %OCTAVE_COPYRIGHT%

#include <string>

extern OCTINTERP_API std::string octave_www_statement (bool html = false);

extern OCTINTERP_API std::string octave_contrib_statement (bool html = false);

extern OCTINTERP_API std::string octave_bugs_statement (bool html = false);

extern OCTINTERP_API std::string octave_name_version_and_copyright (void);

extern OCTINTERP_API std::string
octave_name_version_copyright_copying_and_warranty
  (bool html = false, const std::string& extra_info = std::string ());

extern OCTINTERP_API std::string
octave_name_version_copyright_copying_warranty_and_bugs
  (bool html = false, const std::string& extra_info = std::string ());

extern OCTINTERP_API std::string octave_startup_message (bool html = false);

#endif

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

// Written by John C. Campbell <jcc@bevo.che.wisc.edu>

#if !defined (octave_file_io_h)
#define octave_file_io_h 1

extern OCTINTERP_API void initialize_file_io (void);

extern OCTINTERP_API void close_files (void);

extern OCTINTERP_API void mark_for_deletion (const std::string&);

extern OCTINTERP_API void cleanup_tmp_files (void);

#endif

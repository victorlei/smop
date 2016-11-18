/*

Copyright (C) 2003-2015 John W. Eaton

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

#if !defined (octave_ls_mat5_h)
#define octave_ls_mat5_h 1

enum mat5_data_type
{
  miINT8 = 1,                 // 8 bit signed
  miUINT8,                    // 8 bit unsigned
  miINT16,                    // 16 bit signed
  miUINT16,                   // 16 bit unsigned
  miINT32,                    // 32 bit signed
  miUINT32,                   // 32 bit unsigned
  miSINGLE,                   // IEEE 754 single precision float
  miRESERVE1,
  miDOUBLE,                   // IEEE 754 double precision float
  miRESERVE2,
  miRESERVE3,
  miINT64,                    // 64 bit signed
  miUINT64,                   // 64 bit unsigned
  miMATRIX,                   // MATLAB array
  miCOMPRESSED,               // Compressed data
  miUTF8,                     // Unicode UTF-8 Encoded Character Data
  miUTF16,                    // Unicode UTF-16 Encoded Character Data
  miUTF32                     // Unicode UTF-32 Encoded Character Data
};

extern int
read_mat5_binary_file_header (std::istream& is, bool& swap,
                              bool quiet = false,
                              const std::string& filename = std::string ());
extern std::string
read_mat5_binary_element (std::istream& is, const std::string& filename,
                          bool swap, bool& global, octave_value& tc);
extern bool
save_mat5_binary_element (std::ostream& os,
                          const octave_value& tc, const std::string& name,
                          bool mark_as_global, bool mat7_format,
                          bool save_as_floats, bool compressing = false);

#endif

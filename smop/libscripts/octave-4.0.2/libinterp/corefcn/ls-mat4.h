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

#if !defined (octave_ls_mat4_h)
#define octave_ls_mat4_h 1

extern oct_mach_info::float_format
mopt_digit_to_float_format (int mach);

extern int
float_format_to_mopt_digit (oct_mach_info::float_format flt_fmt);

extern int
read_mat_file_header (std::istream& is, bool& swap, int32_t& mopt,
                      int32_t& nr, int32_t& nc, int32_t& imag,
                      int32_t& len, int quiet = 0);

extern std::string
read_mat_binary_data (std::istream& is, const std::string& filename,
                      octave_value& tc);

extern bool
save_mat_binary_data (std::ostream& os, const octave_value& tc,
                      const std::string& name) ;

#endif

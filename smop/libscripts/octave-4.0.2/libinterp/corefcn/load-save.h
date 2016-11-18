/*

Copyright (C) 1994-2015 John W. Eaton

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

#if !defined (octave_load_save_h)
#define octave_load_save_h 1

#include <iosfwd>
#include <string>

#include "mach-info.h"
#include "symtab.h"

class octave_value;

// FIXME: maybe MAT5 and MAT7 should be options to MAT_BINARY.
// Similarly, save_as_floats may be an option for LS_BINARY, LS_HDF5 etc.
enum load_save_format_type
{
  LS_ASCII,
  LS_BINARY,
  LS_MAT_ASCII,
  LS_MAT_BINARY,
  LS_MAT5_BINARY,
  LS_MAT7_BINARY,
  LS_HDF5,
  LS_UNKNOWN
};

enum load_save_format_options
{
  // LS_MAT_ASCII options (not exclusive)
  LS_MAT_ASCII_LONG = 1,
  LS_MAT_ASCII_TABS = 2,
  // LS_MAT_BINARY options
  LS_MAT_BINARY_V5 = 1,
  LS_MAT_BINARY_V7,
  // zero means no option.
  LS_NO_OPTION = 0
};

class load_save_format
{
public:
  load_save_format (load_save_format_type t,
                    load_save_format_options o = LS_NO_OPTION)
    : type (t), opts (o) { }
  operator int (void) const
  { return type; }
  int type, opts;
};

extern void dump_octave_core (void);

extern int
read_binary_file_header (std::istream& is, bool& swap,
                         oct_mach_info::float_format& flt_fmt,
                         bool quiet = false);

extern octave_value
do_load (std::istream& stream, const std::string& orig_fname,
         load_save_format format, oct_mach_info::float_format flt_fmt,
         bool list_only, bool swap, bool verbose,
         const string_vector& argv, int argv_idx, int argc, int nargout);

extern OCTINTERP_API bool is_octave_data_file (const std::string& file);

extern void
do_save (std::ostream& os, const symbol_table::symbol_record& sr,
         load_save_format fmt, bool save_as_floats);

extern void
write_header (std::ostream& os, load_save_format format);

extern void octave_prepare_hdf5 (void);

extern void octave_finalize_hdf5 (void);

#endif

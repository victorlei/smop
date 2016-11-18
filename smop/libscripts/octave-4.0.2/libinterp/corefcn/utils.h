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

#if !defined (octave_utils_h)
#define octave_utils_h 1

#include <cstdarg>

#include <iosfwd>
#include <string>
#include <list>

#include "dMatrix.h"
#include "lo-utils.h"

#include "cutils.h"

class octave_value;
class octave_value_list;
class string_vector;

extern OCTINTERP_API bool valid_identifier (const char *s);
extern OCTINTERP_API bool valid_identifier (const std::string& s);

extern OCTINTERP_API bool
same_file (const std::string& f, const std::string& g);

extern OCTINTERP_API int almost_match (const std::string& std,
                                       const std::string& s,
                                       int min_match_len = 1,
                                       int case_sens = 1);

extern OCTINTERP_API int
keyword_almost_match (const char * const *std, int *min_len,
                      const std::string& s, int min_toks_to_match,
                      int max_toks);

extern OCTINTERP_API int empty_arg (const char *name, octave_idx_type nr,
                                    octave_idx_type nc);

extern OCTINTERP_API std::string
search_path_for_file (const std::string&, const string_vector&);

extern OCTINTERP_API string_vector
search_path_for_all_files (const std::string&, const string_vector&);

extern OCTINTERP_API std::string
file_in_path (const std::string&, const std::string&);

extern OCTINTERP_API std::string
find_data_file_in_load_path  (const std::string& fcn,
                              const std::string& file,
                              bool require_regular_file = false);

extern OCTINTERP_API std::string contents_file_in_path (const std::string&);

extern OCTINTERP_API std::string fcn_file_in_path (const std::string&);
extern OCTINTERP_API std::string oct_file_in_path (const std::string&);
extern OCTINTERP_API std::string mex_file_in_path (const std::string&);

extern OCTINTERP_API std::string do_string_escapes (const std::string& s);

extern OCTINTERP_API const char *undo_string_escape (char c);

extern OCTINTERP_API std::string undo_string_escapes (const std::string& s);

extern OCTINTERP_API void
check_dimensions (dim_vector& dim, const char *warnfor);

extern OCTINTERP_API void
get_dimensions (const octave_value& a, const char *warn_for,
                dim_vector& dim);

extern OCTINTERP_API void
get_dimensions (const octave_value& a, const octave_value& b,
                const char *warn_for, octave_idx_type& nr,
                octave_idx_type& nc);

extern OCTINTERP_API void
get_dimensions (const octave_value& a,const char *warn_for,
                octave_idx_type& nr, octave_idx_type& nc);

extern OCTINTERP_API octave_idx_type
dims_to_numel (const dim_vector& dims, const octave_value_list& idx);

extern OCTINTERP_API Matrix
identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API FloatMatrix
float_identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API size_t
octave_format (std::ostream& os, const char *fmt, ...);

extern OCTINTERP_API size_t
octave_vformat (std::ostream& os, const char *fmt, va_list args);

extern OCTINTERP_API std::string
octave_vasprintf (const char *fmt, va_list args);

extern OCTINTERP_API std::string octave_asprintf (const char *fmt, ...);

extern OCTINTERP_API void octave_sleep (double seconds);

extern OCTINTERP_API
octave_value_list
do_simple_cellfun (octave_value_list (*fun) (const octave_value_list&, int),
                   const char *fun_name, const octave_value_list& args,
                   int nargout);

extern OCTINTERP_API
octave_value
do_simple_cellfun (octave_value_list (*fun) (const octave_value_list&, int),
                   const char *fun_name, const octave_value_list& args);

class
octave_preserve_stream_state
{
public:

  octave_preserve_stream_state (std::ios& s)
    : stream (s), oflags (s.flags ()), oprecision (s.precision ()),
      owidth (s.width ()), ofill (s.fill ())
  { }

  ~octave_preserve_stream_state (void);

private:

  std::ios& stream;
  std::ios::fmtflags oflags;
  std::streamsize oprecision;
  int owidth;
  char ofill;
};

#endif

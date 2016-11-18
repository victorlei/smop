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

#if !defined (octave_file_ops_h)
#define octave_file_ops_h 1

#include <string>

#include <sys/types.h>

#include "str-vec.h"

struct
OCTAVE_API
file_ops
{
public:

  // Use a singleton class for dir_sep data members instead of just
  // making them static members of the dir_path class so that we can
  // ensure proper initialization.

  file_ops (char dir_sep_char_arg = 0,
            const std::string& dir_sep_str_arg = std::string ("/"),
            const std::string& dir_sep_chars_arg = std::string ("/"))
    : xdir_sep_char (dir_sep_char_arg), xdir_sep_str (dir_sep_str_arg),
      xdir_sep_chars (dir_sep_chars_arg) { }

  typedef std::string (*tilde_expansion_hook) (const std::string&);

  static tilde_expansion_hook tilde_expansion_preexpansion_hook;

  static tilde_expansion_hook tilde_expansion_failure_hook;

  static string_vector tilde_additional_prefixes;

  static string_vector tilde_additional_suffixes;

  static char dir_sep_char (void)
  {
    return instance_ok () ? instance->xdir_sep_char : 0;
  }

  static std::string dir_sep_str (void)
  {
    return instance_ok () ? instance->xdir_sep_str : std::string ();
  }

  static std::string dir_sep_chars (void)
  {
    return instance_ok () ? instance->xdir_sep_chars : std::string ();
  }

  static bool is_dir_sep (char c)
  {
    std::string tmp = dir_sep_chars ();
    return tmp.find (c) != std::string::npos;
  }

  static std::string tilde_expand (const std::string&);

  static string_vector tilde_expand (const string_vector&);

  static std::string concat (const std::string&, const std::string&);

  // Return the tail member of a file name.
  static std::string tail (const std::string& path)
  {
    size_t ipos = path.find_last_of (dir_sep_chars ());

    if (ipos != std::string::npos)
      ipos++;
    else
      ipos = 0;

    return path.substr (ipos);
  }

  // convert path from UNIX type separators to whatever is the system separators
  static std::string native_separator_path (const std::string& path);

private:

  static file_ops *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // No copying!

  file_ops (const file_ops&);

  file_ops& operator = (const file_ops&);

  static bool instance_ok (void);

  char xdir_sep_char;
  std::string xdir_sep_str;
  std::string xdir_sep_chars;
};

// We don't have these in the file_ops class with their simple names
// (i.e., mkdir instead of octave_mdir) because function names in
// standard headers may be #defined.

extern OCTAVE_API int
octave_mkdir (const std::string& nm, mode_t md);

extern OCTAVE_API int
octave_mkdir (const std::string& nm, mode_t md, std::string& msg);

extern OCTAVE_API int
octave_mkfifo (const std::string& nm, mode_t md);

extern OCTAVE_API int
octave_mkfifo (const std::string& nm, mode_t md, std::string& msg);

extern OCTAVE_API int
octave_link (const std::string&, const std::string&);

extern OCTAVE_API int
octave_link (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
octave_symlink (const std::string&, const std::string&);

extern OCTAVE_API int
octave_symlink (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
octave_readlink (const std::string&, std::string&);

extern OCTAVE_API int
octave_readlink (const std::string&, std::string&, std::string&);

extern OCTAVE_API int
octave_rename (const std::string&, const std::string&);

extern OCTAVE_API int
octave_rename (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
octave_rmdir (const std::string&);

extern OCTAVE_API int
octave_rmdir (const std::string&, std::string&);

extern OCTAVE_API int
octave_recursive_rmdir (const std::string&);

extern OCTAVE_API int
octave_recursive_rmdir (const std::string&, std::string&);

extern OCTAVE_API int
octave_umask (mode_t);

extern OCTAVE_API int
octave_unlink (const std::string&);

extern OCTAVE_API int
octave_unlink (const std::string&, std::string&);

extern OCTAVE_API std::string
octave_tempnam (const std::string&, const std::string&);

extern OCTAVE_API std::string
octave_tempnam (const std::string&, const std::string&, std::string&);

extern OCTAVE_API std::string
octave_canonicalize_file_name (const std::string&);

extern OCTAVE_API std::string
octave_canonicalize_file_name (const std::string&, std::string&);

#endif

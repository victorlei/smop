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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <string>

#include <fnmatch.h>
#include <glob.h>

#include "oct-glob.h"
#include "file-stat.h"

// These functions are defined here and not in glob_match.cc so that we
// can include the glob.h file from gnulib, which defines glob to
// be rpl_glob.  If we include glob.h in glob_match.cc, then it
// transforms the glob_match::glob function to be glob_match::rpl_glob,
// which is not what we want...

static bool
single_match_exists (const std::string& file)
{
  file_stat s (file);

  return s.exists ();
}

bool
octave_fnmatch (const string_vector& pat, const std::string& str,
                int fnmatch_flags)
{
  int npat = pat.length ();

  const char *cstr = str.c_str ();

  for (int i = 0; i < npat; i++)
    if (fnmatch (pat(i).c_str (), cstr, fnmatch_flags) != FNM_NOMATCH)
      return true;

  return false;
}

string_vector
octave_glob (const string_vector& pat)
{
  string_vector retval;

  int npat = pat.length ();

  int k = 0;

  for (int i = 0; i < npat; i++)
    {
      std::string xpat = pat(i);

      if (! xpat.empty ())
        {
          glob_t glob_info;

#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) \
          && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM)
          std::replace_if (xpat.begin (), xpat.end (),
                           std::bind2nd (std::equal_to<char> (), '\\'),
                           '/');
#endif

          int err = gnulib::glob (xpat.c_str (), GLOB_NOSORT, 0, &glob_info);

          if (! err)
            {
              int n = glob_info.gl_pathc;

              const char * const *matches = glob_info.gl_pathv;

              // FIXME: we shouldn't have to check to see if
              // a single match exists, but it seems that glob() won't
              // check for us unless the pattern contains globbing
              // characters.  Hmm.

              if (n > 1
                  || (n == 1
                      && single_match_exists (std::string (matches[0]))))
                {
                  retval.resize (k+n);

                  for (int j = 0; j < n; j++)
                    {
                      std::string tmp = matches[j];

#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) \
                      && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM)
                      std::replace_if (tmp.begin (), tmp.end (),
                                       std::bind2nd (std::equal_to<char> (),
                                                     '/'),
                                       '\\');
#endif

                      retval[k++] = tmp;
                    }
                }

              gnulib::globfree (&glob_info);
            }
        }
    }

  return retval.sort ();
}

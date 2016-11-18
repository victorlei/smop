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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <string>

#include "lo-utils.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "singleton-cleanup.h"
#include "str-vec.h"

#include "kpse.cc"

dir_path::static_members *dir_path::static_members::instance = 0;

dir_path::static_members::static_members (void)
  : xpath_sep_char (SEPCHAR), xpath_sep_str (SEPCHAR_STR) { }

bool
dir_path::static_members::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new static_members ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create dir_path::static_members object!");

      retval = false;
    }

  return retval;
}

string_vector
dir_path::elements (void)
{
  return initialized ? pv : string_vector ();
}

string_vector
dir_path::all_directories (void)
{
  int count = 0;
  string_vector retval;

  if (initialized)
    {
      int len = pv.length ();

      int nmax = len > 32 ? len : 32;

      retval.resize (len);

      for (int i = 0; i < len; i++)
        {
          str_llist_type *elt_dirs = kpse_element_dirs (pv[i]);

          if (elt_dirs)
            {
              str_llist_elt_type *dir;

              for (dir = *elt_dirs; dir; dir = STR_LLIST_NEXT (*dir))
                {
                  const std::string elt_dir = STR_LLIST (*dir);

                  if (! elt_dir.empty ())
                    {
                      if (count == nmax)
                        nmax *= 2;

                      retval.resize (nmax);

                      retval[count++] = elt_dir;
                    }
                }
            }
        }

      retval.resize (count);
    }

  return retval;
}

std::string
dir_path::find_first (const std::string& nm)
{
  return initialized ? kpse_path_search (p, nm, true) : std::string ();
}

string_vector
dir_path::find_all (const std::string& nm)
{
  return initialized ? kpse_all_path_search (p, nm) : string_vector ();
}

std::string
dir_path::find_first_of (const string_vector& names)
{
  return initialized
         ? kpse_path_find_first_of (p, names, true) : std::string ();
}

string_vector
dir_path::find_all_first_of (const string_vector& names)
{
  return initialized
         ? kpse_all_path_find_first_of (p, names) : string_vector ();
}

void
dir_path::init (void)
{
  static bool octave_kpathsea_initialized = false;

  if (! octave_kpathsea_initialized)
    {
      std::string val = octave_env::getenv ("KPATHSEA_DEBUG");

      if (! val.empty ())
        kpathsea_debug |= atoi (val.c_str ());

      octave_kpathsea_initialized = true;
    }

  p = kpse_path_expand (p_default.empty ()
                        ? p_orig : kpse_expand_default (p_orig, p_default));

  int count = 0;
  for (kpse_path_iterator pi (p); pi != std::string::npos; pi++)
    count++;

  pv.resize (count);

  kpse_path_iterator pi (p);

  for (int i = 0; i < count; i++)
    pv[i] = *pi++;

  initialized = true;
}

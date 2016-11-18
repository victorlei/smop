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

#include <cerrno>
#include <cstdlib>
#include <cstring>

#include <list>
#include <string>

#include "sysdir.h"

#include "dir-ops.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-sysdep.h"
#include "str-vec.h"

bool
dir_entry::open (const std::string& n)
{
  fail = true;

  if (! n.empty ())
    name = n;

  if (! name.empty ())
    {
      close ();

      std::string fullname = file_ops::tilde_expand (name);

      dir = static_cast<void *> (gnulib::opendir (fullname.c_str ()));

      if (dir)
        fail = false;
      else
        errmsg = gnulib::strerror (errno);
    }
  else
    errmsg = "dir_entry::open: empty file name";

  return ! fail;
}

string_vector
dir_entry::read (void)
{
  string_vector retval;

  if (ok ())
    {
      std::list<std::string> dirlist;

      struct dirent *dir_ent;

      while ((dir_ent = gnulib::readdir (static_cast<DIR *> (dir))))
        {
          if (dir_ent)
            dirlist.push_back (dir_ent->d_name);
          else
            break;
        }

      retval = string_vector (dirlist);
    }

  return retval;
}

void
dir_entry::close (void)
{
  if (dir)
    gnulib::closedir (static_cast<DIR *> (dir));

  dir = 0;
}

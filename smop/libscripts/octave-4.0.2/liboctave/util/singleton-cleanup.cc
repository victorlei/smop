/*

Copyright (C) 2012-2015 John W. Eaton

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

#include <lo-error.h>
#include <singleton-cleanup.h>

singleton_cleanup_list *singleton_cleanup_list::instance = 0;

singleton_cleanup_list::~singleton_cleanup_list (void)
{
  for (std::set<fptr>::iterator p = fcn_list.begin ();
       p != fcn_list.end (); p++)
    {
      fptr fcn = *p;

      fcn ();
    }
}

bool
singleton_cleanup_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new singleton_cleanup_list ();

  if (! instance)
    {
      current_liboctave_error_handler
        ("unable to create singleton_cleanup_list object!");

      retval = false;
    }

  return retval;
}

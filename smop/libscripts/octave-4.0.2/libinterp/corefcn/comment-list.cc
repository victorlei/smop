/*

Copyright (C) 2000-2015 John W. Eaton

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

#include "lo-utils.h"

#include "comment-list.h"
#include "error.h"

octave_comment_list *
octave_comment_list::dup (void) const
{
  octave_comment_list *new_cl = new octave_comment_list ();

  for (const_iterator p = begin (); p != end (); p++)
    {
      const octave_comment_elt elt = *p;

      new_cl->append (elt);
    }

  return new_cl;
}

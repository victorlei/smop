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

#if !defined (octave_dir_ops_h)
#define octave_dir_ops_h 1

#include <string>

#include "str-vec.h"

class
OCTAVE_API
dir_entry
{
public:

  dir_entry (const std::string& n = std::string ())
    : name (n), dir (0), fail (false), errmsg ()
  {
    if (! name.empty ())
      open ();
  }

  dir_entry (const dir_entry& d)
    : name (d.name), dir (d.dir), fail (d.fail), errmsg (d.errmsg) { }

  dir_entry& operator = (const dir_entry& d)
  {
    if (this != &d)
      {
        name = d.name;
        dir = d.dir;
        fail = d.fail;
        errmsg = d.errmsg;
      }

    return *this;
  }

  ~dir_entry (void) { close (); }

  bool open (const std::string& = std::string ());

  string_vector read (void);

  void close (void);

  bool ok (void) const { return dir && ! fail; }

  operator bool () const { return ok (); }

  std::string error (void) const { return ok () ? std::string () : errmsg; }

private:

  // Name of the directory.
  std::string name;

  // A pointer to the contents of the directory.  We use void here to
  // avoid possible conflicts with the way some systems declare the
  // type DIR.
  void *dir;

  // TRUE means the open for this directory failed.
  bool fail;

  // If a failure occurs, this contains the system error text.
  std::string errmsg;
};

#endif

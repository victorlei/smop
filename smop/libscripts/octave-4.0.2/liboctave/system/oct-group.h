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

#if !defined (octave_oct_group_h)
#define octave_oct_group_h 1

#include <string>

#include <sys/types.h>

#include "str-vec.h"

class
OCTAVE_API
octave_group
{
public:

  octave_group (void)
    : gr_name (), gr_passwd (), gr_gid (0), gr_mem (), valid (false)
  { }

  octave_group (const octave_group& gr)
    : gr_name (gr.gr_name), gr_passwd (gr.gr_passwd),
      gr_gid (gr.gr_gid), gr_mem (gr.gr_mem), valid (gr.valid)
  { }

  octave_group& operator = (const octave_group& gr)
  {
    if (this != &gr)
      {
        gr_name = gr.gr_name;
        gr_passwd = gr.gr_passwd;
        gr_gid = gr.gr_gid;
        gr_mem = gr.gr_mem;
        valid = gr.valid;
      }

    return *this;
  }

  std::string name (void) const;

  std::string passwd (void) const;

  gid_t gid (void) const;

  string_vector mem (void) const;

  bool ok (void) const { return valid; }

  operator bool () const { return ok (); }

  static octave_group getgrent (void);
  static octave_group getgrent (std::string& msg);

  static octave_group getgrgid (gid_t gid);
  static octave_group getgrgid (gid_t gid, std::string& msg);

  static octave_group getgrnam (const std::string& nm);
  static octave_group getgrnam (const std::string& nm, std::string& msg);

  static int setgrent (void);
  static int setgrent (std::string& msg);

  static int endgrent (void);
  static int endgrent (std::string& msg);

private:

  // The group name.
  std::string gr_name;

  // The group password.
  std::string gr_passwd;

  // The numeric group id.
  gid_t gr_gid;

  // The members of the group;
  string_vector gr_mem;

  // Flag that says whether we have been properly initialized.
  bool valid;

  // This is how we will create an octave_group object from a pointer
  // to a struct group.
  octave_group (void *p, std::string& msg);

  void gripe_invalid (void) const;
};

#endif

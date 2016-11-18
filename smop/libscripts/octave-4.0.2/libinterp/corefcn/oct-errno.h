// oct-errno.h.in
/*

Copyright (C) 2005-2015 John W. Eaton

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

#if !defined (octave_oct_errno_h)
#define octave_oct_errno_h 1

#include <cerrno>
#include <map>
#include <string>

#include "oct-map.h"

class
octave_errno
{
protected:

  octave_errno (void);

public:

  ~octave_errno (void) { }

  static bool instance_ok (void);

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static int lookup (const std::string& name);

  static octave_scalar_map list (void);

  static int get (void) { return errno; }

  static int set (int val)
  {
    int retval = errno;
    errno = val;
    return retval;
  }

private:

  std::map<std::string, int> errno_tbl;

  static octave_errno *instance;

  int do_lookup (const std::string& name);

  octave_scalar_map do_list (void);
};

#endif

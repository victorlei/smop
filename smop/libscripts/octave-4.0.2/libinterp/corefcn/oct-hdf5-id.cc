/*

Copyright (C) 2015 John W. Eaton

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

#include "error.h"
#include "oct-hdf5.h"
#include "oct-hdf5-id.h"

bool
check_hdf5_id_type (bool warn)
{
  static bool checked = false;
  static bool ok = false;

  if (! checked)
    {
#if defined (HAVE_HDF5)
      ok = sizeof (octave_hdf5_id) >= sizeof (hid_t);

      if (warn && ! ok)
        warning_with_id
          ("Octave:internal",
           "the size of octave_hdf5_id is smaller than the size of HDF5 hid_t");
#else
      warning_with_id
        ("Octave:internal",
         "check_hdf5_id_type called but Octave was not compiled with support for HDF5");
#endif

      checked = true;
    }

  return ok;
}

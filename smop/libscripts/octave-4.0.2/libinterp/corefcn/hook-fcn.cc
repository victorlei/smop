/*

Copyright (C) 2013-2015 John W. Eaton

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

#include <config.h>

#include "hook-fcn.h"

hook_function::hook_function (const octave_value& f, const octave_value& d)
{
  if (f.is_string ())
    {
      std::string name = f.string_value ();

      rep = new named_hook_function (name, d);
    }
  else if (f.is_function_handle ())
    {
      rep = new fcn_handle_hook_function (f, d);
    }
  else
    error ("invalid hook function");
}

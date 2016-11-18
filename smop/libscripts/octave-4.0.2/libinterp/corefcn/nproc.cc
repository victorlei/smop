/*

Copyright (C) 2012-2015 Iain Murray

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

#include "defun.h"
#include "nproc.h"

DEFUN (nproc, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} nproc ()\n\
@deftypefnx {Built-in Function} {} nproc (@var{query})\n\
Return the current number of available processors.\n\
\n\
If called with the optional argument @var{query}, modify how processors\n\
are counted as follows:\n\
\n\
@table @code\n\
@item all\n\
total number of processors.\n\
\n\
@item current\n\
processors available to the current process.\n\
\n\
@item overridable\n\
same as @code{current}, but overridable through the @w{@env{OMP_NUM_THREADS}}\n\
environment variable.\n\
@end table\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if ((nargin != 0 && nargin != 1) || (nargout != 0 && nargout != 1))
    {
      print_usage ();
      return retval;
    }

  nproc_query query = NPROC_CURRENT;
  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      std::transform (arg.begin (), arg.end (), arg.begin (), tolower);

      if (arg == "all")
        query = NPROC_ALL;
      else if (arg == "current")
        query = NPROC_CURRENT;
      else if (arg == "overridable")
        query = NPROC_CURRENT_OVERRIDABLE;
      else
        {
          error ("nproc: invalid value for QUERY");
          return retval;
        }
    }

  retval = num_processors (query);

  return retval;
}

/*
## Must always report at least 1 cpu available
%!assert (nproc () >= 1);
%!assert (nproc ("all") >= 1);
%!assert (nproc ("current") >= 1);

%!test
%! c = nproc ("current");
%! unwind_protect
%!   old_val = getenv ("OMP_NUM_THREADS");
%!   new_val = c + 1;
%!   setenv ("OMP_NUM_THREADS", num2str (new_val));
%!   assert (nproc ("overridable"), new_val);
%! unwind_protect_cleanup
%!   if (! isempty (old_val))
%!     setenv ("OMP_NUM_THREADS", old_val);
%!   else
%!     unsetenv ("OMP_NUM_THREADS");
%!   endif
%! end_unwind_protect

%!error nproc ("no_valid_option");
*/

/*

Copyright (C) 2007-2015 David Bateman


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

#include <string>
#include <vector>

#include "defun.h"
#include "file-stat.h"
#include "file-ops.h"
#include "load-path.h"
#include "oct-env.h"
#include "oct-md5.h"
#include "utils.h"

DEFUN (md5sum, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} md5sum (@var{file})\n\
@deftypefnx {Built-in Function} {} md5sum (@var{str}, @var{opt})\n\
Calculate the MD5 sum of the file @var{file}.\n\
\n\
If the second parameter @var{opt} exists and is true, then calculate the MD5\n\
sum of the string @var{str}.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    print_usage ();
  else
    {
      bool have_str = false;
      std::string str = args(0).string_value ();

      if (nargin == 2)
        have_str = args(1).bool_value ();

      if (!error_state)
        {
          if (have_str)
            retval = oct_md5 (str);
          else
            {
              std::string fname = file_ops::tilde_expand (str);

              fname = find_data_file_in_load_path ("md5sum", fname);

              retval = oct_md5_file (fname);
            }
        }
    }

  return retval;
}

/*
%!assert (md5sum ("abc\0", true), "147a664a2ca9410911e61986d3f0d52a");

%!test
%! tfile = tempname ();
%! fid = fopen (tfile, "wb");
%! fwrite (fid, "abc\0");
%! fclose (fid);
%! assert (md5sum (tfile), "147a664a2ca9410911e61986d3f0d52a");
%! unlink (tfile);

%!error md5sum ();
*/


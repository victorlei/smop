## Copyright (C) 2010-2015 VZLU Prague
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{str} =} fileread (@var{filename})
## Read the contents of @var{filename} and return it as a string.
## @seealso{fread, textread, sscanf}
## @end deftypefn

function str = fileread (filename)

  if (nargin != 1)
    print_usage ();
  endif

  if (! ischar (filename))
    error ("fileread: FILENAME argument must be a string");
  endif

  fid = fopen (filename, "r");
  if (fid < 0)
    error ("fileread: cannot open file");
  endif

  unwind_protect
    str = (fread (fid, "*char")).';
  unwind_protect_cleanup
    fclose (fid);
  end_unwind_protect

endfunction


%!test
%! cstr = {"Hello World", "The answer is 42", "Goodbye World"};
%! fname = tempname ();
%! fid = fopen (fname, "w");
%! fprintf (fid, "%s\n", cstr{:});
%! fclose (fid);
%! str = fileread (fname);
%! unlink (fname);
%! assert (str, [cstr{1} "\n" cstr{2} "\n" cstr{3} "\n"]);

## Test input validation
%!error fileread ()
%!error fileread (1, 2)
%!error <FILENAME argument must be a string> fileread (1)


## Copyright (C) 2010-2015 John W. Eaton
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
## @deftypefn {Function File} {} is_valid_file_id (@var{fid})
## Return true if @var{fid} refers to an open file.
## @seealso{freport, fopen}
## @end deftypefn

function retval = is_valid_file_id (fid)

  retval = false;

  if (nargin == 1)
    try
      if (isscalar (fid))
        [file, mode, arch] = fopen (fid);
        retval = ! isempty (file);
      endif
    end_try_catch
  else
    print_usage ();
  endif

endfunction


%!assert (is_valid_file_id (stdout))
%!assert (! is_valid_file_id ([1,2;3,4]))
%!assert (! is_valid_file_id ("not_a_file_id"))
%!assert (! is_valid_file_id (-1))
%!assert (! is_valid_file_id (pi))


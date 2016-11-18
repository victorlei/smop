## Copyright (C) 2004-2015 Alois Schloegl
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
## @deftypefn {Function File} {} isdir (@var{f})
## Return true if @var{f} is a directory.
## @seealso{exist, stat, is_absolute_filename, is_rooted_relative_filename}
## @end deftypefn

function retval = isdir (f)
  if (nargin != 1)
    print_usage ("isdir");
  endif

  ## Exist returns an integer but isdir should return a logical.
  retval = (exist (f, "dir") == 7);

endfunction


%!assert (isdir (pwd ()))
%!assert (! isdir ("this is highly unlikely to be a directory name"))

%!error isdir ()
%!error isdir (1, 2)


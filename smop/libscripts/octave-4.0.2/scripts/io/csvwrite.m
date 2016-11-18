## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} csvwrite (@var{filename}, @var{x})
## @deftypefnx {Function File} {} csvwrite (@var{filename}, @var{x}, @var{dlm_opts})
## Write the matrix @var{x} to the file @var{filename} in
## @w{comma-separated-value} format.
##
## This function is equivalent to
##
## @example
## dlmwrite (@var{filename}, @var{x}, ",", @dots{})
## @end example
##
## @seealso{csvread, dlmwrite, dlmread}
## @end deftypefn

function csvwrite (filename, x, varargin)
  dlmwrite (filename, x, ",", varargin{:});
endfunction


%!shared fname
%! fname = tempname ();

%!test
%! csvwrite (fname, magic (3));
%! data = csvread (fname);
%! unlink (fname);
%! assert (data, magic (3));

%!test
%! csvwrite (fname, magic (3), "precision", "%2.1f", "newline", "unix");
%! fid = fopen (fname, "rt");
%! txt = char (fread (fid,Inf,'char')');
%! fclose (fid);
%! unlink (fname);
%! assert (txt, "8.0,1.0,6.0\n3.0,5.0,7.0\n4.0,9.0,2.0\n");


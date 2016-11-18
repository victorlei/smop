## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn {Function File} {} int2str (@var{n})
## Convert an integer (or array of integers) to a string (or a character
## array).
##
## @example
## @group
## int2str (123)
##      @result{} "123"
##
## s = int2str ([1, 2, 3; 4, 5, 6])
##      @result{} s =
##         1  2  3
##         4  5  6
##
## whos s
##      @result{}
##       Attr Name        Size                     Bytes  Class
##       ==== ====        ====                     =====  =====
##            s           2x7                         14  char
## @end group
## @end example
##
## This function is not very flexible.  For better control over the
## results, use @code{sprintf} (@pxref{Formatted Output}).
## @seealso{sprintf, num2str, mat2str}
## @end deftypefn

## Author: jwe

function retval = int2str (n)

  if (nargin != 1)
    print_usage ();
  endif

  if (isempty (n))
    retval = '';
    return;
  endif

  n = round (real (n));
  sz = size (n);
  nd = ndims (n);
  nc = columns (n);
  if (nc > 1)
    idx = repmat ({':'}, nd, 1);
    idx(2) = 1;
    ifmt = get_fmt (n(idx{:}), 0);
    idx(2) = 2:sz(2);
    rfmt = get_fmt (n(idx{:}), 2);
    fmt = [ifmt repmat(rfmt,1,nc-1) "\n"];
  else
    fmt = [get_fmt(n, 0) "\n"];
  endif
  tmp = sprintf (fmt, permute (n, [2, 1, 3 : nd]));
  tmp(end) = "";
  retval = char (ostrsplit (tmp, "\n"));

endfunction

function fmt = get_fmt (x, sep)

  t = x(:);
  t = t(t != 0);
  if (isempty (t))
    ## All zeros.
    fmt = sprintf ("%%%dd", 1 + sep);
  else
    ## Maybe have some zeros.
    nan_inf = ! isfinite (t);
    if (any (nan_inf))
      if (any (t(nan_inf) < 0))
        min_fw = 4 + sep;
      else
        min_fw = 3 + sep;
      endif
    else
      min_fw = 1 + sep;
    endif
    t = t(! nan_inf);
    if (isempty (t))
      ## Only zeros, Inf, and NaN.
      fmt = sprintf ("%%%dd", min_fw);
    else
      ## Could have anything.
      tfw = floor (log10 (double (abs (t)))) + 1 + sep;
      fw = max (tfw);
      if (any (t(tfw == fw) < 0))
        fw++;
      endif
      fmt = sprintf ("%%%dd", max (fw, min_fw));
    endif
  endif

endfunction


%!assert (int2str (-123), "-123")
%!assert (int2str (1.2), "1")
%!assert (int2str ([1, 2, 3; 4, 5, 6]), ["1  2  3";"4  5  6"])
%!assert (int2str ([]), "")

%!error int2str ()
%!error int2str (1, 2)


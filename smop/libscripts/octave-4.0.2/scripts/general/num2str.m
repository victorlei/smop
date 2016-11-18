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
## @deftypefn  {Function File} {} num2str (@var{x})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number (or array) to a string (or a character array).
##
## The optional second argument may either give the number of significant
## digits (@var{precision}) to be used in the output or a format template
## string (@var{format}) as in @code{sprintf} (@pxref{Formatted Output}).
## @code{num2str} can also process complex numbers.
##
## Examples:
##
## @example
## @group
## num2str (123.456)
##      @result{} "123.46"
##
## num2str (123.456, 4)
##      @result{} "123.5"
##
## s = num2str ([1, 1.34; 3, 3.56], "%5.1f")
##      @result{} s =
##         1.0  1.3
##         3.0  3.6
## whos s
##      @result{}
##       Attr Name        Size                     Bytes  Class
##       ==== ====        ====                     =====  =====
##            s           2x8                         16  char
##
## num2str (1.234 + 27.3i)
##      @result{} "1.234+27.3i"
## @end group
## @end example
##
## Notes:
##
## For @sc{matlab} compatibility, leading spaces are stripped before returning
## the string.
##
## The @code{num2str} function is not very flexible.  For better control
## over the results, use @code{sprintf} (@pxref{Formatted Output}).
##
## For complex @var{x}, the format string may only contain one output
## conversion specification and nothing else.  Otherwise, results will be
## unpredictable.
## @seealso{sprintf, int2str, mat2str}
## @end deftypefn

## Author: jwe

function retval = num2str (x, arg)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  elseif (! (isnumeric (x) || islogical (x) || ischar (x)))
    error ("num2str: X must be a numeric, logical, or character array");
  endif

  if (ischar (x))
    retval = x;
  elseif (isempty (x))
    retval = "";
  elseif (isreal (x))
    if (nargin == 2)
      if (ischar (arg))
        fmt = arg;
      elseif (isnumeric (arg) && isscalar (arg) && arg >= 0 && arg == fix (arg))
        fmt = sprintf ("%%%d.%dg", arg+7, arg);
      else
        error ("num2str: PRECISION must be a scalar integer >= 0");
      endif
    else
      if (isnumeric (x))
        ## Setup a suitable format string, ignoring inf entries
        dgt = floor (log10 (max (abs (x(! isinf (x(:)))))));
        if (isempty (dgt))
          ## If the whole input array is inf...
          dgt = 1;
        endif

        if (any (x(:) != fix (x(:))))
          ## Floating point input
          dgt = max (dgt + 4, 5);   # Keep 4 sig. figures after decimal point
          dgt = min (dgt, 16);      # Cap significant digits at 16
          fmt = sprintf ("%%%d.%dg", dgt+7+any (x(:) < 0), dgt);
        else
          ## Integer input
          dgt = max (dgt + 1, 1);
          ## FIXME: Integers should be masked to show only 16 significant digits
          ##        See %!xtest below
          fmt = sprintf ("%%%d.%dg", dgt+2+any (x(:) < 0), dgt);
        endif
      else
        ## Logical input
        fmt = "%3d";
      endif
    endif
    fmt = [deblank(repmat(fmt, 1, columns(x))), "\n"];
    nd = ndims (x);
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));
    retval = strtrim (char (ostrsplit (tmp(1:end-1), "\n")));
  else   # Complex matrix input
    if (nargin == 2)
      if (ischar (arg))
        fmt = [arg "%-+" arg(2:end) "i"];
      elseif (isnumeric (arg) && isscalar (arg) && arg >= 0 && arg == fix (arg))
        fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", arg+7, arg, arg+7, arg);
      else
        error ("num2str: PRECISION must be a scalar integer >= 0");
      endif
    else
      ## Setup a suitable format string
      dgt = floor (log10 (max (max (abs (real (x(! isinf (real (x(:))))))),
                               max (abs (imag (x(! isinf (imag (x(:))))))))));
      if (isempty (dgt))
        ## If the whole input array is inf...
        dgt = 1;
      endif

      if (any (x(:) != fix (x(:))))
        ## Floating point input
          dgt = max (dgt + 4, 5);   # Keep 4 sig. figures after decimal point
          dgt = min (dgt, 16);      # Cap significant digits at 16
          fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", dgt+7, dgt, dgt+7, dgt);
      else
        ## Integer input
        dgt = max (1 + dgt, 1);
        ## FIXME: Integers should be masked to show only 16 significant digits
        ##        See %!xtest below
        fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", dgt+2, dgt, dgt+2, dgt);
      endif
    endif

    ## Manipulate the complex value to have real values in the odd
    ## columns and imaginary values in the even columns.
    nc = columns (x);
    nd = ndims (x);
    idx = repmat ({':'}, nd, 1);
    perm(1:2:2*nc) = 1:nc;
    perm(2:2:2*nc) = nc + (1:nc);
    idx{2} = perm;
    x = horzcat (real (x), imag (x));
    x = x(idx{:});

    fmt = [deblank(repmat(fmt, 1, nc)), "\n"];
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));

    ## Put the "i"'s where they are supposed to be.
    tmp = regexprep (tmp, " +i\n", "i\n");
    tmp = regexprep (tmp, "( +)i", "i$1");

    retval = strtrim (char (ostrsplit (tmp(1:end-1), "\n")));
  endif

endfunction


%!assert (num2str (123), "123")
%!assert (num2str (1.23), "1.23")
%!assert (num2str (123.456, 4), "123.5")
%!assert (num2str ([1, 1.34; 3, 3.56], "%5.1f"),  ["1.0  1.3"; "3.0  3.6"])
%!assert (num2str (1.234 + 27.3i), "1.234+27.3i")
%!assert (num2str ([true false true]), "1  0  1");

%!assert (num2str (19440606), "19440606")
%!assert (num2str (2^33), "8589934592")
%!assert (num2str (-2^33), "-8589934592")
%!assert (num2str (2^33+1i), "8589934592+1i")
%!assert (num2str (-2^33+1i), "-8589934592+1i")
%!assert (num2str (inf), "Inf")
%!assert (num2str ([inf -inf]), "Inf -Inf")
%!assert (num2str ([complex(Inf,0), complex(0,-Inf)]), "Inf+0i   0-Infi")
%!assert (num2str (complex(Inf,1)), "Inf+1i")
%!assert (num2str (complex(1,Inf)), "1+Infi")
%!assert (num2str (nan), "NaN")
%!assert (num2str (complex (NaN, 1)), "NaN+1i")
%!assert (num2str (complex (1, NaN)), "1+NaNi")
%!assert (num2str (NA), "NA")
%!assert (num2str (complex (NA, 1)), "NA+1i")
%!assert (num2str (complex (1, NA)), "1+NAi")

## FIXME: Integers greater than bitmax() should be masked to show just
##        16 digits of precision.
%!xtest
%! assert (num2str (1e23), "100000000000000000000000");

%!error num2str ()
%!error num2str (1, 2, 3)
%!error <X must be a numeric> num2str ({1})
%!error <PRECISION must be a scalar integer> num2str (1, {1})
%!error <PRECISION must be a scalar integer> num2str (1, ones (2))
%!error <PRECISION must be a scalar integer> num2str (1, -1)
%!error <PRECISION must be a scalar integer> num2str (1+1i, {1})
%!error <PRECISION must be a scalar integer> num2str (1+1i, ones (2))
%!error <PRECISION must be a scalar integer> num2str (1+1i, -1)


## Copyright (C) 2010-2015 Rik Wehbring
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
## @deftypefn  {Function File} {} randi (@var{imax})
## @deftypefnx {Function File} {} randi (@var{imax}, @var{n})
## @deftypefnx {Function File} {} randi (@var{imax}, @var{m}, @var{n}, @dots{})
## @deftypefnx {Function File} {} randi ([@var{imin} @var{imax}], @dots{})
## @deftypefnx {Function File} {} randi (@dots{}, "@var{class}")
## Return random integers in the range 1:@var{imax}.
##
## Additional arguments determine the shape of the return matrix.  When no
## arguments are specified a single random integer is returned.  If one
## argument @var{n} is specified then a square matrix @w{(@var{n} x @var{n})}
## is returned.  Two or more arguments will return a multi-dimensional matrix
## @w{(@var{m} x @var{n} x @dots{})}.
##
## The integer range may optionally be described by a two element matrix with a
## lower and upper bound in which case the returned integers will be on the
## interval @w{[@var{imin}, @var{imax}]}.
##
## The optional argument @var{class} will return a matrix of the requested
## type.  The default is @qcode{"double"}.
##
## The following example returns 150 integers in the range 1--10.
##
## @example
## ri = randi (10, 150, 1)
## @end example
##
## Implementation Note: @code{randi} relies internally on @code{rand} which
## uses class @qcode{"double"} to represent numbers.  This limits the maximum
## integer (@var{imax}) and range (@var{imax} - @var{imin}) to the value
## returned by the @code{flintmax} function.  For IEEE floating point numbers
## this value is @w{@math{2^{53} - 1}}.
##
## @seealso{rand}
## @end deftypefn

## Author: Rik Wehbring

function ri = randi (bounds, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (bounds) && all (bounds == fix (bounds))))
    error ("randi: IMIN and IMAX must be integer bounds");
  endif

  bounds = real (double (bounds));
  if (isscalar (bounds))
    imin = 1;
    imax = bounds;
    if (imax < 1)
      error ("randi: require IMAX >= 1");
    endif
  else
    imin = bounds(1);
    imax = bounds(2);
    if (imax < imin)
      error ("randi: require IMIN <= IMAX");
    endif
  endif

  if (nargin > 1 && ischar (varargin{end}))
    rclass = varargin{end};
    varargin(end) = [];
  else
    rclass = "double";
  endif

  ## Limit set by use of class double in rand(): Any consecutive integer in the
  ## range [-flintmax(), flintmax()] can be represented by a double.
  if ((abs (imax) >= flintmax ()) || (abs (imin) >= flintmax ()))
    error ("randi: IMIN and IMAX must be smaller than flintmax()");
  endif
  if ((imax - imin) >= (flintmax () - 1))
    error ("randi: integer range must be smaller than flintmax()-1");
  endif

  ri = imin + floor ((imax - imin + 1) * rand (varargin{:}));

  if (! strcmp (rclass, "double"))
    if (strfind (rclass, "int"))
      maxval = double (intmax (rclass));
      minval = double (intmin (rclass));
    elseif (strcmp (rclass, "single"))
      maxval = double (flintmax (rclass));
      minval = -maxval;
    else
      error ("randi: unknown requested output class '%s'", rclass);
    endif
    if (imax > maxval)
      warning (["randi: integer IMAX exceeds requested type.  ", ...
                "Values might be truncated to requested type."]);
    elseif (imin < minval)
      warning (["randi: integer IMIN exceeds requested type.  ", ...
                " Values might be truncated to requested type."]);
    endif

    ri = cast (ri, rclass);
  endif

endfunction


%!test
%! ri = randi (10, 1000, 1);
%! assert (ri, fix (ri));
%! assert (min (ri), 1);
%! assert (max (ri), 10);
%! assert (rows (ri), 1000);
%! assert (columns (ri), 1);
%! assert (class (ri), "double");
## FIXME: Does Octave guarantee support for int64 even when underlying hardware
##        is 32-bit?
%!test
%! ri = randi (int64 (100), 1, 1000);
%! assert (ri, fix (ri));
%! assert (min (ri), 1);
%! assert (max (ri), 100);
%! assert (rows (ri), 1);
%! assert (columns (ri), 1000);
%! assert (class (ri), "double");
%!test
%! ri = randi ([-5, 10], 1000, 1, "int8");
%! assert (ri, fix (ri));
%! assert (min (ri), int8 (-5));
%! assert (max (ri), int8 (10));
%! assert (class (ri), "int8");
%!test
%! ri = randi ([-5; 10], 1000, 1, "single");
%! assert (ri, fix (ri));
%! assert (min (ri), single (-5));
%! assert (max (ri), single (10));
%! assert (class (ri), "single");

%!assert (size (randi (10, 3, 1, 2)), [3, 1, 2])

%!shared max_int8, min_int8, max_uint8, min_uint8, max_single
%! max_int8 = double (intmax ("int8"));
%! min_int8 = double (intmin ("int8"));
%! max_uint8 = double (intmax ("uint8"));
%! min_uint8 = double (intmin ("uint8"));
%! max_single = double (flintmax ("single"));

## Test that no warning thrown if IMAX is exactly on the limits of the range
%!function test_no_warning (func, varargin)
%! state = warning ("query");
%! unwind_protect
%!   warning ("error", "all");
%!   func (varargin{:});
%! unwind_protect_cleanup
%!   warning (state);
%! end_unwind_protect
%!endfunction
%!test test_no_warning (@randi, max_int8, "int8");
%!test test_no_warning (@randi, max_uint8, "uint8");
%!test test_no_warning (@randi, max_single, "single");
%!test test_no_warning (@randi, [min_int8, max_int8], "int8");
%!test test_no_warning (@randi, [min_uint8, max_uint8], "uint8");
%!test test_no_warning (@randi, [-max_single, max_single], "single");

## Test exceeding range
%!warning <exceeds requested type>
%! randi ([min_int8-1, max_int8], "int8");
%!warning <exceeds requested type>
%! randi ([min_uint8-1, max_uint8], "uint8");
%!warning <exceeds requested type>
%! randi ([min_int8, max_int8 + 1], "int8");
%!warning <exceeds requested type>
%! randi ([min_uint8, max_uint8 + 1], "uint8");
%!warning <exceeds requested type>
%! randi ([0, max_single + 1], "single");
%!warning <exceeds requested type>
%! ri = randi ([-5, 10], 1000, 1, "uint8");
%! assert (ri, fix (ri));
%! assert (min (ri), uint8 (-5));
%! assert (max (ri), uint8 (10));
%! assert (class (ri), "uint8");


## Test input validation
%!error randi ()
%!error <must be integer bounds> randi ("test")
%!error <must be integer bounds> randi (struct ("a", 1))
%!error <must be integer bounds> randi (1.5)
%!error <must be integer bounds> randi ([1.5, 2.5])
%!error <must be integer bounds> randi ([1, 2.5])
%!error <must be integer bounds> randi ([1.5, 2])
%!error <require IMAX .= 1> randi (0)
%!error <require IMIN <= IMAX> randi ([10, 1])
%!error <IMIN and IMAX must be smaller than flintmax\(\)> randi (flintmax ())
%!error <range must be smaller than flintmax\(\)-1> randi ([-1, flintmax() - 1])
%!error <unknown requested output class 'foo'> randi (10, "foo")


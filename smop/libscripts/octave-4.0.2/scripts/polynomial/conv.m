## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} conv (@var{a}, @var{b})
## @deftypefnx {Function File} {} conv (@var{a}, @var{b}, @var{shape})
## Convolve two vectors @var{a} and @var{b}.
##
## The output convolution is a vector with length equal to
## @code{length (@var{a}) + length (@var{b}) - 1}.
## When @var{a} and @var{b} are the coefficient vectors of two polynomials, the
## convolution represents the coefficient vector of the product polynomial.
##
## The optional @var{shape} argument may be
##
## @table @asis
## @item @var{shape} = @qcode{"full"}
## Return the full convolution.  (default)
##
## @item @var{shape} = @qcode{"same"}
## Return the central part of the convolution with the same size as @var{a}.
## @end table
##
## @seealso{deconv, conv2, convn, fftconv}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function y = conv (a, b, shape = "full")

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (a) && isvector (b)))
    error ("conv: both arguments A and B must be vectors");
  elseif (nargin == 3 && ! any (strcmpi (shape, {"full", "same", "valid"})))
    error ('conv: SHAPE argument must be "full", "same", or "valid"');
  endif

  la = la_orig = length (a);
  lb = lb_orig = length (b);

  ly = la + lb - 1;

  if (ly == 0)
    y = zeros (1, 0);
    return;
  endif

  ## Use shortest vector as the coefficent vector to filter.
  if (la > lb)
    [a, b] = deal (b, a);  # Swap vectors
    lb = la;
  endif
  x = b;

  ## Pad longer vector to convolution length.
  if (ly > lb)
    x(end+1:end+ly-lb) = 0;
  endif

  y = filter (a, 1, x);

  if (strcmpi (shape, "same"))
    idx = ceil ((ly - la) / 2);
    y = y(idx+1:idx+la);
  elseif (strcmpi (shape, "valid"))
    len = la_orig - lb_orig;
    y = y(lb_orig:lb_orig+len);
  endif

endfunction


%!test
%! x = ones (3,1);
%! y = ones (1,3);
%! b = 2;
%! c = 3;
%! assert (conv (x, x), [1; 2; 3; 2; 1]);
%! assert (conv (y, y), [1, 2, 3, 2, 1]);
%! assert (conv (x, y), [1, 2, 3, 2, 1]);
%! assert (conv (y, x), [1; 2; 3; 2; 1]);
%! assert (conv (c, x), [3; 3; 3]);
%! assert (conv (c, y), [3, 3, 3]);
%! assert (conv (x, c), [3; 3; 3]);
%! assert (conv (y, c), [3, 3, 3]);
%! assert (conv (b, c), 6);

%!shared a,b
%!test
%! a = 1:10;
%! b = 1:3;
%!assert (size (conv (a,b)), [1, numel(a)+numel(b)-1])
%!assert (size (conv (b,a)), [1, numel(a)+numel(b)-1])

%!test
%! a = (1:10).';
%!assert (size (conv (a,b)), [numel(a)+numel(b)-1, 1])
%!assert (size (conv (b,a)), [numel(a)+numel(b)-1, 1])

%!test
%! a = 1:10;
%! b = (1:3).';
%!assert (size (conv (a,b)), [1, numel(a)+numel(b)-1])
%!assert (size (conv (b,a)), [1, numel(a)+numel(b)-1])

%!test
%! a = 1:10;
%! b = 1:3;

%!assert (conv (a,b,"full"), conv (a,b))
%!assert (conv (b,a,"full"), conv (b,a))

%!assert (conv (a,b,"same"), [4, 10, 16, 22, 28, 34, 40, 46, 52, 47])
%!assert (conv (b,a,"same"), [28, 34, 40])

%!assert (conv (a,b,"valid"), [10, 16, 22, 28, 34, 40, 46, 52])
%!assert (conv (b,a,"valid"), zeros (1,0))


## Test input validation
%!error conv (1)
%!error conv (1,2,3,4)
%!error <A and B must be vectors> conv ([1, 2; 3, 4], 3)
%!error <A and B must be vectors> conv (3, [1, 2; 3, 4])
%!error <SHAPE argument must be> conv (2, 3, "INVALID_SHAPE")


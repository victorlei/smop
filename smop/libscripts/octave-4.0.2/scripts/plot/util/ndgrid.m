## Copyright (C) 2006-2015 Alexander Barth
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
## @deftypefn  {Function File} {[@var{y1}, @var{y2}, @dots{}, @var{y}n] =} ndgrid (@var{x1}, @var{x2}, @dots{}, @var{x}n)
## @deftypefnx {Function File} {[@var{y1}, @var{y2}, @dots{}, @var{y}n] =} ndgrid (@var{x})
## Given n vectors @var{x1}, @dots{}, @var{x}n, @code{ndgrid} returns n
## arrays of dimension n.
##
## The elements of the i-th output argument contains the elements of the
## vector @var{x}i repeated over all dimensions different from the i-th
## dimension.  Calling ndgrid with only one input argument @var{x} is
## equivalent to calling ndgrid with all n input arguments equal to @var{x}:
##
## [@var{y1}, @var{y2}, @dots{}, @var{y}n] = ndgrid (@var{x}, @dots{}, @var{x})
##
## Programming Note: @code{ndgrid} is very similar to the function
## @code{meshgrid} except that the first two dimensions are transposed in
## comparison to @code{meshgrid}.  Some core functions expect @code{meshgrid}
## input and others expect @code{ndgrid} input.  Check the documentation for
## the function in question to determine the proper input format.
## @seealso{meshgrid}
## @end deftypefn

## Author: Alexander Barth <abarth@marine.usf.edu>

function varargout = ndgrid (varargin)

  if (nargin == 0)
    print_usage ();
  elseif (nargin == 1)
    nd = max (nargout, 1);
    ## If only one input argument is given, repeat it nd-times
    varargin(1:nd) = varargin(1);
  elseif (nargin >= nargout)
    nd = max (nargin, 1);
  else
    error ("ndgrid: wrong number of input arguments");
  endif

  ## Determine the size of the output arguments
  shape = zeros (1, nd);
  for i = 1:nd
    if (! isvector (varargin{i}) && ! isempty (varargin{i}))
      error ("ndgrid: arguments must be vectors");
    endif
    shape(i) = length (varargin{i});
  endfor

  if (nd == 1)
    ## Special case, single input vector
    varargout{1} = varargin{1}(:);
  else
    for i = 1:nd
      ## size for reshape
      r = ones (1, nd+1);
      r(i) = shape(i);

      ## size for repmat
      s = shape;
      s(i) = 1;

      varargout{i} = repmat (reshape (varargin{i}, r), s);
    endfor
  endif

endfunction


%!test
%! x = 1:3;
%! assert (isequal (ndgrid (x), x(:)));

%!test
%! x = 1:3;
%! [XX, YY] = ndgrid (x);
%! assert (size_equal (XX, YY));
%! assert (isequal (XX, repmat(x(:), 1, numel(x))));
%! assert (isequal (YY, repmat(x, numel(x), 1)));

%!test
%! x = 1:2;
%! y = 1:3;
%! z = 1:4;
%! [XX, YY, ZZ] = ndgrid (x, y, z);
%! assert (size_equal (XX, YY, ZZ));
%! assert (ndims (XX), 3);
%! assert (size (XX), [2, 3, 4]);
%! assert (XX(1) * YY(1) * ZZ(1), x(1) * y(1) * z(1));
%! assert (XX(end) * YY(end) * ZZ(end), x(end) * y(end) * z(end));

%!test
%! x = 1:2;
%! y = 1:3;
%! [XX1, YY1] = meshgrid (x, y);
%! [XX2, YY2] = ndgrid (x, y);
%! assert (size_equal (XX1, YY1));
%! assert (size_equal (XX2, YY2));
%! assert (ndims (XX1), 2);
%! assert (size (XX1), [3, 2]);
%! assert (size (XX2), [2, 3]);
%! assert (XX2(1) * YY2(1), x(1) * y(1));
%! assert (XX2(end) * YY2(end), x(end) * y(end));
%! assert (XX1, XX2.');
%! assert (YY1, YY2.');

%!assert (ndgrid ([]), zeros(0,1))
%!assert (ndgrid ([], []), zeros(0,0))

## Test input validation
%!error ndgrid ()
%!error <wrong number of input arguments> [a,b,c] = ndgrid (1:3,1:3)
%!error <arguments must be vectors> ndgrid (ones (2,2))


## Copyright (C) 1995-2015 Kurt Hornik
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn {Function File} {} vech (@var{x})
## Return the vector obtained by eliminating all superdiagonal elements of
## the square matrix @var{x} and stacking the result one column above the
## other.
##
## This has uses in matrix calculus where the underlying matrix is symmetric
## and it would be pointless to keep values above the main diagonal.
## @seealso{vec}
## @end deftypefn

## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.

## Author KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 May 1995
## Adapted-By: jwe

function v = vech (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (! issquare (x))
    error ("vech: X must be square");
  endif

  n = rows (x);
  slices = cellslices (x(:), (1:n) + n*(0:n-1), n*(1:n));
  v = vertcat (slices{:});

endfunction


%!assert (vech ([1, 2, 3; 4, 5, 6; 7, 8, 9]), [1; 4; 7; 5; 8; 9])

%!error vech ()
%!error vech (1, 2)


## Copyright (C) 2004-2015 Paul Kienzle
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn {Function File} {} inputname (@var{n})
## Return the name of the @var{n}-th argument to the calling function.
##
## If the argument is not a simple variable name, return an empty string.
## @code{inputname} may only be used within a function body, not at the
## command line.
## @seealso{nargin, nthargout}
## @end deftypefn

function s = inputname (n)

  if (nargin != 1)
    print_usage ();
  endif

  s = evalin ("caller", sprintf ("__varval__ (\".argn.\"){%d};", fix (n)));
  ## For compatibility with Matlab,
  ## return empty string if argument name is not a valid identifier.
  if (! isvarname (s))
    s = "";
  endif

endfunction


## Warning: heap big magic in the following tests!!!
## The test function builds a private context for each
## test, with only the specified values shared between
## them.  It does this using the following template:
##
##     function [<shared>] = testfn(<shared>)
##        <test>
##
## To test inputname, I need a function context invoked
## with known parameter names.  So define a couple of
## shared parameters, et voila!, the test is trivial.

%!shared hello, worldly
%!assert (inputname (1), "hello")
%!assert (inputname (2), "worldly")

%!function r = foo (x, y)
%!  r = inputname (2);
%!endfunction
%!assert (foo (pi, e), "e");
%!assert (feval (@foo, pi, e), "e");

%!error inputname ()
%!error inputname (1,2)


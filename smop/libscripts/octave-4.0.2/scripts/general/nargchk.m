## Copyright (C) 2008-2015 Bill Denney
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
## @deftypefn  {Function File} {@var{msgstr} =} nargchk (@var{minargs}, @var{maxargs}, @var{nargs})
## @deftypefnx {Function File} {@var{msgstr} =} nargchk (@var{minargs}, @var{maxargs}, @var{nargs}, "string")
## @deftypefnx {Function File} {@var{msgstruct} =} nargchk (@var{minargs}, @var{maxargs}, @var{nargs}, "struct")
## Return an appropriate error message string (or structure) if the number of
## inputs requested is invalid.
##
## This is useful for checking to see that the number of input arguments
## supplied to a function is within an acceptable range.
##
## @strong{Caution}: @code{nargchk} is scheduled for deprecation.  Use
## @code{narginchk} in all new code.
## @seealso{narginchk, nargoutchk, error, nargin, nargout}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function msg = nargchk (minargs, maxargs, nargs, outtype = "string")

  if (nargin < 3 || nargin > 4)
    print_usage ();
  elseif (minargs > maxargs)
    error ("nargchk: MINARGS must be <= MAXARGS");
  elseif (! any (strcmpi (outtype, {"string", "struct"})))
    error ('nargchk: output type must be either "string" or "struct"');
  elseif (! (isscalar (minargs) && isscalar (maxargs) && isscalar (nargs)))
    error ("nargchk: MINARGS, MAXARGS, and NARGS must be scalars");
  endif

  msg = struct ("message", "", "identifier", "");
  if (nargs < minargs)
    msg.message = "not enough input arguments";
    msg.identifier = "Octave:nargchk:not-enough-inputs";
  elseif (nargs > maxargs)
    msg.message = "too many input arguments";
    msg.identifier = "Octave:nargchk:too-many-inputs";
  endif

  if (strcmpi (outtype, "string"))
    msg = msg.message;
  elseif (isempty (msg.message))
    ## Compatability: Matlab returns a 0x1 empty struct when nargchk passes
    msg = resize (msg, 0, 1);
  endif

endfunction


## Tests
%!shared stnul, stmin, stmax
%! stnul = resize (struct ("message", "", "identifier", ""), 0, 1);
%! stmin = struct ("message", "not enough input arguments",
%!                 "identifier", "Octave:nargchk:not-enough-inputs");
%! stmax = struct ("message", "too many input arguments",
%!                 "identifier", "Octave:nargchk:too-many-inputs");
%!assert (nargchk (0, 1, 0), "")
%!assert (nargchk (0, 1, 1), "")
%!assert (nargchk (1, 1, 0), "not enough input arguments")
%!assert (nargchk (0, 1, 2), "too many input arguments")
%!assert (nargchk (0, 1, 2, "string"), "too many input arguments")
## Struct outputs
%!assert (nargchk (0, 1, 0, "struct"), stnul)
%!assert (nargchk (0, 1, 1, "struct"), stnul)
%!assert (nargchk (1, 1, 0, "struct"), stmin)
%!assert (nargchk (0, 1, 2, "struct"), stmax)


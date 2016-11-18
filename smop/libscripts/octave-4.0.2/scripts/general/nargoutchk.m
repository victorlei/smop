## Copyright (C) 2008-2015 Bill Denney
## Copyright (C) 2012 Carnë Draug
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
## @deftypefn  {Function File} {} nargoutchk (@var{minargs}, @var{maxargs})
## @deftypefnx {Function File} {@var{msgstr} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs})
## @deftypefnx {Function File} {@var{msgstr} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs}, "string")
## @deftypefnx {Function File} {@var{msgstruct} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs}, "struct")
## Check for correct number of output arguments.
##
## In the first form, return an error if the number of arguments is not between
## @var{minargs} and @var{maxargs}.  Otherwise, do nothing.  Note that this
## function evaluates the value of @code{nargout} on the caller so its value
## must have not been tampered with.
##
## Both @var{minargs} and @var{maxargs} must be numeric scalars.  Zero, Inf,
## and negative are all valid, and they can have the same value.
##
## For backwards compatibility, the other forms return an appropriate error
## message string (or structure) if the number of outputs requested is
## invalid.
##
## This is useful for checking to that the number of output arguments supplied
## to a function is within an acceptable range.
## @seealso{narginchk, error, nargout, nargin}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>
## Author: Carnë Draug <carandraug+dev@gmail.com>

function msg = nargoutchk (minargs, maxargs, nargs, outtype)

  ## before matlab's 2011b, nargoutchk would return an error message (just the
  ## message in a string). With 2011b, it no longer returns anything, it simply
  ## gives an error if the args number is incorrect.
  ## To try to keep compatibility with both versions, check nargout and nargin
  ## to guess if the caller is expecting a value (old syntax)
  ## or none (new syntax).

  if (nargout == 1 && (nargin == 3 || nargin == 4))

    if (minargs > maxargs)
      error ("nargoutchk: MINARGS must be <= MAXARGS");
    elseif (nargin == 3)
      outtype = "string";
    elseif (! any (strcmpi (outtype, {"string" "struct"})))
      error ("nargoutchk: output type must be either string or struct");
    elseif (! (isscalar (minargs) && isscalar (maxargs) && isscalar (nargs)))
      error ("nargoutchk: MINARGS, MAXARGS, and NARGS must be scalars");
    endif

    msg = struct ("message", "", "identifier", "");
    if (nargs < minargs)
      msg.message = "not enough output arguments";
      msg.identifier = "Octave:nargoutchk:not-enough-outputs";
    elseif (nargs > maxargs)
      msg.message = "too many output arguments";
      msg.identifier = "Octave:nargoutchk:too-many-outputs";
    endif

    if (strcmpi (outtype, "string"))
      msg = msg.message;
    elseif (isempty (msg.message))
      ## Compatibility: Matlab returns a 0x1 empty struct when nargoutchk passes
      msg = resize (msg, 0, 1);
    endif

  elseif (nargout == 0 && nargin == 2)

    if (! isnumeric (minargs) || ! isscalar (minargs))
      error ("minargs must be a numeric scalar");
    elseif (! isnumeric (maxargs) || ! isscalar (maxargs))
      error ("maxargs must be a numeric scalar");
    elseif (minargs > maxargs)
      error ("minargs cannot be larger than maxargs");
    endif

    args = evalin ("caller", "nargout;");

    if (args < minargs)
      error ("Not enough output arguments.");
    elseif (args > maxargs)
      error ("Too many output arguments.");
    endif

  else
    print_usage;
  endif

endfunction


%!shared stnul, stmin, stmax
%! stnul = resize (struct ("message", "", "identifier", ""), 0, 1);
%! stmin = struct ("message", "not enough output arguments",
%!                 "identifier", "Octave:nargoutchk:not-enough-outputs");
%! stmax = struct ("message", "too many output arguments",
%!                 "identifier", "Octave:nargoutchk:too-many-outputs");
%!assert (nargoutchk (0, 1, 0), "")
%!assert (nargoutchk (0, 1, 1), "")
%!assert (nargoutchk (1, 1, 0), "not enough output arguments")
%!assert (nargoutchk (0, 1, 2), "too many output arguments")
%!assert (nargoutchk (0, 1, 2, "string"), "too many output arguments")
## Struct outputs
%!assert (nargoutchk (0, 1, 0, "struct"), stnul)
%!assert (nargoutchk (0, 1, 1, "struct"), stnul)
%!assert (nargoutchk (1, 1, 0, "struct"), stmin)
%!assert (nargoutchk (0, 1, 2, "struct"), stmax)


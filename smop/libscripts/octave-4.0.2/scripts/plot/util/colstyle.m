## Copyright (C) 2012-2015 David Bateman
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
## @deftypefn {Function File} {[@var{style}, @var{color}, @var{marker}, @var{msg}] =} colstyle (@var{linespec})
## Parse @var{linespec} and return the line style, color, and markers given.
##
## In the case of an error, the string @var{msg} will return the text of the
## error.
## @end deftypefn

function [l, c, m, msg] = colstyle (style)

  if (nargin != 1)
    print_usage ();
  endif

  if (! ischar (style))
    error ("colstyle: STYLE must be a string");
  endif

  try
    opt = __pltopt__ ("colstyle", style);
    l = opt.linestyle;
    switch (opt.color)
      case [0 0 0]
        c = "k";
      case [1 0 0]
        c = "r";
      case [0 1 0]
        c = "g";
      case [0 0 1]
        c = "b";
      case [1 1 0]
        c = "y";
      case [1 0 1]
        c = "m";
      case [0 1 1]
        c = "c";
      case [0 1 1]
        c = "w";
      otherwise
        c = opt.color;
    endswitch
    m = opt.marker;
    msg = [];
  catch
    l = c = m = [];
    msg = lasterr ();
  end_try_catch

endfunction


%!test
%! [l, c, m, msg] = colstyle ("r:x");
%! assert (isempty (msg));
%! assert (l, ":");
%! assert (c, "r");
%! assert (m, "x");

%!test
%! [l, c, m, msg] = colstyle (".");
%! assert (isempty (msg));
%! assert (l, "none");
%! assert (c, []);
%! assert (m, ".");

%!test
%! [l, c, m, msg] = colstyle ("~");
%! assert (msg, "colstyle: unrecognized format character: '~'");

## Test input validation
%!error colstyle ()
%!error colstyle (1, 2)
%!error <STYLE must be a string> colstyle (1.5)


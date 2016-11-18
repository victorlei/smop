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
## @deftypefn {Function File} {} __pltopt__ (@var{caller}, @var{opt})
## Undocumented internal function.
## @end deftypefn

## @deftypefn {Function File} {} __pltopt__ (@var{caller}, @var{opt})
##
## Decode plot option strings.
##
## @var{opt} can currently be some combination of the following:
##
## @table @code
## @item "-"
## For solid linestyle (default).
##
## @item "--"
## For dashed line style.
##
## @item "-."
## For linespoints plot style.
##
## @item ":"
## For dots plot style.
##
## @item "r"
## Red line color.
##
## @item "g"
## Green line color.
##
## @item "b"
## Blue line color.
##
## @item "c"
## Cyan line color.
##
## @item "m"
## Magenta line color.
##
## @item "y"
## Yellow line color.
##
## @item "k"
## Black line color.
##
## @item "w"
## White line color.
##
## @item ";title;"
## Here @code{"title"} is the label for the key.
##
## @item "+"
## @itemx "o"
## @itemx "*"
## @itemx "."
## @itemx "x"
## @itemx "s"
## @itemx "d"
## @itemx "^"
## @itemx "v"
## @itemx ">"
## @itemx "<"
## @itemx "p"
## @itemx "h"
## Used in combination with the points or linespoints styles, set the point
## style.
## @end table
##
## The legend may be fixed to include the name of the variable
## plotted in some future version of Octave.

## Author: jwe

function [options, valid] = __pltopt__ (caller, opt, err_on_invalid = true)

  options = __default_plot_options__ ();
  valid = true;

  if (ischar (opt))
    opt = cellstr (opt);
  elseif (! iscellstr (opt))
    error ("__pltopt__: argument must be a character string or cell array of character strings");
  endif

  nel = numel (opt);

  for i = nel:-1:1
    [options(i), valid] = __pltopt1__ (caller, opt{i}, err_on_invalid);
    if (! err_on_invalid && ! valid)
      return;
    endif
  endfor

endfunction

## Really decode plot option strings.

## Author: Rick Niles <niles@axp745.gsfc.nasa.gov>
## Adapted-By: jwe
## Maintainer: jwe

function [options, valid] = __pltopt1__ (caller, opt, err_on_invalid)

  options = __default_plot_options__ ();
  valid = true;

  have_linestyle = false;
  have_marker = false;

  ## If called by __errplot__, extract the linestyle before proceeding.
  if (strcmp (caller, "__errplot__"))
    if (strncmp (opt, "#~>", 3))
      n = 3;
    elseif (strncmp (opt, "#~", 2) || strncmp (opt, "~>", 2))
      n = 2;
    elseif (strncmp (opt, "~", 1) || strncmp (opt, ">", 1)
            || strncmp (opt, "#", 1))
      n = 1;
    else
      n = 0;
    endif
    options.errorstyle = opt(1:n);
    opt(1:n) = [];
  else
    options.errorstyle = "~";
  endif

  while (! isempty (opt))
    if (strncmp (opt, "--", 2) || strncmp (opt, "-.", 2))
      options.linestyle = opt(1:2);
      have_linestyle = true;
      n = 2;
    else
      topt = opt(1);
      n = 1;
      if (topt == "-" || topt == ":")
        have_linestyle = true;
        options.linestyle = topt;
      elseif (topt == "+" || topt == "o" || topt == "*"
              || topt == "." || topt == "x" || topt == "s"
              || topt == "d" || topt == "^" || topt == "v"
              || topt == ">" || topt == "<" || topt == "p"
              || topt == "h" || topt == "@")
        have_marker = true;
        ## Check for long form marker styles
        if (any (topt == "sdhp"))
          if (strncmp (opt, "square", 6))
            n = 6;
          elseif (strncmp (opt, "diamond", 7))
            n = 7;
          elseif (strncmp (opt, "hexagram", 8))
            n = 8;
          elseif (strncmp (opt, "pentagram", 9))
            n = 9;
          endif
        endif
        ## Backward compatibility.  Leave undocumented.
        if (topt == "@")
          topt = "+";
        endif
        options.marker = topt;
      ## Numeric color specs are for backward compatibility.  Don't document.
      elseif (topt == "k" || topt == "0")
        options.color = [0, 0, 0];
      elseif (topt == "r" || topt == "1")
        options.color = [1, 0, 0];
      elseif (topt == "g" || topt == "2")
        options.color = [0, 1, 0];
      elseif (topt == "b" || topt == "3")
        options.color = [0, 0, 1];
      elseif (topt == "y")
        options.color = [1, 1, 0];
      elseif (topt == "m" || topt == "4")
        options.color = [1, 0, 1];
      elseif (topt == "c" || topt == "5")
        options.color = [0, 1, 1];
      elseif (topt == "w" || topt == "6")
        options.color = [1, 1, 1];
      elseif (isspace (topt))
        ## Do nothing.
      elseif (topt == ";")
        t = index (opt(2:end), ";");
        if (t)
          options.key = opt(2:t);
          n = t+1;
        else
          if (err_on_invalid)
            error ("%s: unfinished key label", caller);
          else
            valid = false;
            options = __default_plot_options__ ();
            return;
          endif
        endif
      else
        if (err_on_invalid)
          error ("%s: unrecognized format character: '%s'", caller, topt);
        else
          valid = false;
          options = __default_plot_options__ ();
          return;
        endif
      endif
    endif
    opt(1:n) = [];
  endwhile

  if (! have_linestyle && have_marker)
    options.linestyle = "none";
  endif

  if (have_linestyle && ! have_marker)
    options.marker = "none";
  endif

endfunction


## Only cursory testing.  Real testing done by appearance of plots.
%test
%! opts = __pltopt__ ("abc", "");
%! assert (opts.color, []);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);
%! assert (opts.key, "");
%!test
%! opts = __pltopt__ ("abc", "r:x");
%! assert (opts.color, [1 0 0]);
%! assert (opts.linestyle, ":");
%! assert (opts.marker, "x");
%!test
%! opts = __pltopt__ ("abc", "2square");
%! assert (opts.color, [0 1 0]);
%! assert (opts.linestyle, "none");
%! assert (opts.marker, "s");
%!test
%! opts = __pltopt__ ("abc", ";Title;");
%! assert (opts.key, "Title");
%! assert (opts.color, []);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);
%!test
%! opts = __pltopt__ ("__errplot__", "~>r");
%! assert (opts.errorstyle, "~>");
%! assert (opts.color, [1 0 0 ]);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);

## Test input validation
%!error <argument must be a character string or cell array> __pltopt__ ("abc", 1)
%!error <unfinished key label> __pltopt__ ("abc", "rx;my_title", true)
%!error <unrecognized format character: 'u'> __pltopt__ ("abc", "u", true)


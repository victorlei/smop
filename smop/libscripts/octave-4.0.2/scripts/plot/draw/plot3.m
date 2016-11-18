## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Function File} {} plot3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} plot3 (@var{x}, @var{y}, @var{z}, @var{prop}, @var{value}, @dots{})
## @deftypefnx {Function File} {} plot3 (@var{x}, @var{y}, @var{z}, @var{fmt})
## @deftypefnx {Function File} {} plot3 (@var{x}, @var{cplx})
## @deftypefnx {Function File} {} plot3 (@var{cplx})
## @deftypefnx {Function File} {} plot3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} plot3 (@dots{})
## Produce 3-D plots.
##
## Many different combinations of arguments are possible.  The simplest
## form is
##
## @example
## plot3 (@var{x}, @var{y}, @var{z})
## @end example
##
## @noindent
## in which the arguments are taken to be the vertices of the points to
## be plotted in three dimensions.  If all arguments are vectors of the
## same length, then a single continuous line is drawn.  If all arguments
## are matrices, then each column of is treated as a separate line.  No attempt
## is made to transpose the arguments to make the number of rows match.
##
## If only two arguments are given, as
##
## @example
## plot3 (@var{x}, @var{cplx})
## @end example
##
## @noindent
## the real and imaginary parts of the second argument are used
## as the @var{y} and @var{z} coordinates, respectively.
##
## If only one argument is given, as
##
## @example
## plot3 (@var{cplx})
## @end example
##
## @noindent
## the real and imaginary parts of the argument are used as the @var{y}
## and @var{z} values, and they are plotted versus their index.
##
## Arguments may also be given in groups of three as
##
## @example
## plot3 (@var{x1}, @var{y1}, @var{z1}, @var{x2}, @var{y2}, @var{z2}, @dots{})
## @end example
##
## @noindent
## in which each set of three arguments is treated as a separate line or
## set of lines in three dimensions.
##
## To plot multiple one- or two-argument groups, separate each group
## with an empty format string, as
##
## @example
## plot3 (@var{x1}, @var{c1}, "", @var{c2}, "", @dots{})
## @end example
##
## Multiple property-value pairs may be specified which will affect the line
## objects drawn by @code{plot3}.  If the @var{fmt} argument is supplied it
## will format the line objects in the same manner as @code{plot}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## Example:
##
## @example
## @group
## z = [0:0.05:5];
## plot3 (cos (2*pi*z), sin (2*pi*z), z, ";helix;");
## plot3 (z, exp (2i*pi*z), ";complex sinusoid;");
## @end group
## @end example
## @seealso{ezplot3, plot}
## @end deftypefn

## Author: Paul Kienzle
##         (modified from __plt__.m)

function retval = plot3 (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("plot3", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    x_set = 0;
    y_set = 0;
    z_set = 0;
    property_set = 0;
    fmt_set = 0;
    properties = {};
    tlgnd = {};
    hlgnd = [];
    idx = 0;

    ## Gather arguments, decode format, and plot lines.
    arg = 0;
    while (arg++ < nargs)
      new = varargin{arg};
      new_cell = varargin(arg);

      if (property_set)
        properties = [properties, new_cell];
        property_set = 0;
        continue;
      endif

      if (ischar (new))
        if (! z_set)
          if (! y_set)
            if (! x_set)
              error ("plot3: needs X, [ Y, [ Z ] ]");
            else
              y = real (x);
              z = imag (x);
              y_set = 1;
              z_set = 1;
              if (rows (x) > 1)
                x = repmat ((1:rows (x))', 1, columns (x));
              else
                x = 1:columns (x);
              endif
            endif
          else
            z = imag (y);
            y = real (y);
            z_set = 1;
          endif
        endif

        if (! fmt_set)
          [options, valid] = __pltopt__ ("plot3", new, false);
          if (! valid)
            properties = [properties, new_cell];
            property_set = 1;
            continue;
          else
            fmt_set = 1;
            while (arg < nargs && ischar (varargin{arg+1}))
              if (nargs - arg < 2)
                error ("plot3: properties must appear followed by a value");
              endif
              properties = [properties, varargin(arg+1:arg+2)];
              arg += 2;
            endwhile
          endif
        else
          properties = [properties, new_cell];
          property_set = 1;
          continue;
        endif

        if (isvector (x) && isvector (y))
          if (isvector (z))
            x = x(:);
            y = y(:);
            z = z(:);
          elseif (length (x) == rows (z) && length (y) == columns (z))
            [x, y] = meshgrid (x, y);
          else
            error ("plot3: [length(X), length(Y)] must match size (Z)");
          endif
        endif

        if (! size_equal (x, y, z))
          error ("plot3: X, Y, and Z must have the same shape");
        elseif (ndims (x) > 2)
          error ("plot3: X, Y, and Z must not have more than two dimensions");
        endif

        for i = 1 : columns (x)
          linestyle = options.linestyle;
          marker = options.marker;
          if (isempty (marker) && isempty (linestyle))
             [linestyle, marker] = __next_line_style__ ();
          endif
          color = options.color;
          if (isempty (color))
            color = __next_line_color__ ();
          endif

          htmp(++idx) = line (x(:, i), y(:, i), z(:, i),
                              "color", color, "linestyle", linestyle,
                              "marker", marker, properties{:});
          key = options.key;
          if (! isempty (key))
            hlgnd = [hlgnd, htmp(idx)];
            tlgnd = {tlgnd{:}, key};
          endif
        endfor

        x_set = 0;
        y_set = 0;
        z_set = 0;
        fmt_set = 0;
        properties = {};
      elseif (! x_set)
        x = new;
        x_set = 1;
      elseif (! y_set)
        y = new;
        y_set = 1;
      elseif (! z_set)
        z = new;
        z_set = 1;
      else
        if (isvector (x) && isvector (y))
          if (isvector (z))
            x = x(:);
            y = y(:);
            z = z(:);
          elseif (length (x) == rows (z) && length (y) == columns (z))
            [x, y] = meshgrid (x, y);
          else
            error ("plot3: [length(X), length(Y)] must match size (Z)");
          endif
        endif

        if (! size_equal (x, y, z))
          error ("plot3: X, Y, and Z must have the same shape");
        elseif (ndims (x) > 2)
          error ("plot3: X, Y, and Z must not have more than two dimensions");
        endif

        options = __default_plot_options__ ();
        for i = 1 : columns (x)
          linestyle = options.linestyle;
          marker = options.marker;
          if (isempty (marker) && isempty (linestyle))
            [linestyle, marker] = __next_line_style__ ();
          endif
          color = options.color;
          if (isempty (color))
            color = __next_line_color__ ();
          endif

          htmp(++idx) = line (x(:, i), y(:, i), z(:, i),
                              "color", color, "linestyle", linestyle,
                              "marker", marker, properties{:});
          key = options.key;
          if (! isempty (key))
            hlgnd = [hlgnd, htmp(idx)];
            tlgnd = {tlgnd{:}, key};
          endif
        endfor

        x = new;
        y_set = 0;
        z_set = 0;
        fmt_set = 0;
        properties = {};
      endif

    endwhile

    if (property_set)
      error ("plot3: properties must appear followed by a value");
    endif

    ## Handle last plot.

    if (x_set)
      if (y_set)
        if (! z_set)
          z = imag (y);
          y = real (y);
          z_set = 1;
        endif
      else
        y = real (x);
        z = imag (x);
        y_set = 1;
        z_set = 1;
        if (rows (x) > 1)
          x = repmat ((1:rows (x))', 1, columns (x));
        else
          x = 1:columns (x);
        endif
      endif

      if (isvector (x) && isvector (y))
        if (isvector (z))
          x = x(:);
          y = y(:);
          z = z(:);
        elseif (length (x) == rows (z) && length (y) == columns (z))
          [x, y] = meshgrid (x, y);
        else
          error ("plot3: [length(X), length(Y)] must match size (Z)");
        endif
      endif

      if (! size_equal (x, y, z))
        error ("plot3: X, Y, and Z must have the same shape");
      elseif (ndims (x) > 2)
        error ("plot3: X, Y, and Z must not have more than two dimensions");
      endif

      options = __default_plot_options__ ();

      for i = 1 : columns (x)
        linestyle = options.linestyle;
        marker = options.marker;
        if (isempty (marker) && isempty (linestyle))
          [linestyle, marker] = __next_line_style__ ();
        endif
        color = options.color;
        if (isempty (color))
          color = __next_line_color__ ();
        endif

        htmp(++idx) = line (x(:, i), y(:, i), z(:, i),
                            "color", color, "linestyle", linestyle,
                            "marker", marker, properties{:});
        key = options.key;
        if (! isempty (key))
          hlgnd = [hlgnd, htmp(idx)];
          tlgnd = {tlgnd{:}, key};
        endif
      endfor
    endif

    if (! isempty (hlgnd))
      legend (hax, hlgnd, tlgnd);
    endif

    if (! ishold ())
      set (hax, "view", [-37.5, 30]);
    endif

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0 && idx > 0)
    retval = htmp;
  endif

endfunction


%!demo
%! clf;
%! z = [0:0.05:5];
%! plot3 (cos (2*pi*z), sin (2*pi*z), z)
%! legend ('helix');
%! title ('plot3() of a helix');

%!demo
%! clf;
%! z = [0:0.05:5];
%! plot3 (z, exp (2i*pi*z));
%! legend ('complex sinusoid');
%! title ('plot3() with complex input');


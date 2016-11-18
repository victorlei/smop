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
## @deftypefn  {Function File} {} image (@var{img})
## @deftypefnx {Function File} {} image (@var{x}, @var{y}, @var{img})
## @deftypefnx {Function File} {} image (@dots{}, "@var{prop}", @var{val}, @dots{})
## @deftypefnx {Function File} {} image ("@var{prop1}", @var{val1}, @dots{})
## @deftypefnx {Function File} {@var{h} =} image (@dots{})
## Display a matrix as an indexed color image.
##
## The elements of @var{img} are indices into the current colormap.
##
## @var{x} and @var{y} are optional 2-element vectors, @w{@code{[min, max]}},
## which specify the range for the axis labels.  If a range is specified as
## @w{@code{[max, min]}} then the image will be reversed along that axis.  For
## convenience, @var{x} and @var{y} may be specified as N-element vectors
## matching the length of the data in @var{img}.  However, only the first and
## last elements will be used to determine the axis limits.
## @strong{Warning:} @var{x} and @var{y} are ignored when using gnuplot 4.0
## or earlier.
##
## Multiple property/value pairs may be specified for the image object, but
## they must appear in pairs.
##
## The optional return value @var{h} is a graphics handle to the image.
##
## Implementation Note: The origin (0, 0) for images is located in the
## upper left.  For ordinary plots, the origin is located in the lower
## left.  Octave handles this inversion by plotting the data normally,
## and then reversing the direction of the y-axis by setting the
## @code{ydir} property to @qcode{"reverse"}.  This has implications whenever
## an image and an ordinary plot need to be overlaid.  The recommended
## solution is to display the image and then plot the reversed ydata
## using, for example, @code{flipud (ydata)}.
##
## Calling Forms: The @code{image} function can be called in two forms:
## High-Level and Low-Level.  When invoked with normal options, the High-Level
## form is used which first calls @code{newplot} to prepare the graphic figure
## and axes.  When the only inputs to @code{image} are property/value pairs
## the Low-Level form is used which creates a new instance of an image object
## and inserts it in the current axes.
## @seealso{imshow, imagesc, colormap}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = image (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("image", varargin{:});

  chararg = find (cellfun ("isclass", varargin, "char"), 1, "first");

  do_new = true;
  if (nargin == 0)
    img = get (0, "defaultimagecdata");
    x = y = [];
  elseif (chararg == 1)
    ## Low-Level syntax
    do_new = false;
    x = y = img = [];
    idx = find (strcmpi (varargin, "cdata"), 1);
    if (idx)
      img = varargin{idx+1};
      varargin(idx:idx+1) = [];
    endif
    idx = find (strcmpi (varargin, "xdata"), 1);
    if (idx)
      x = varargin{idx+1};
      varargin(idx:idx+1) = [];
    endif
    idx = find (strcmpi (varargin, "ydata"), 1);
    if (idx)
      y = varargin{idx+1};
      varargin(idx:idx+1) = [];
    endif
  elseif (nargin == 1 || chararg == 2)
    img = varargin{1};
    x = y = [];
  elseif (nargin == 2 || chararg == 3)
    print_usage ();
  else
    x = varargin{1};
    y = varargin{2};
    img = varargin{3};
    chararg = 4;
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    if (do_new)
      hax = newplot (hax);
    elseif (isempty (hax))
      hax = gca ();
    else
      hax = hax(1);
    endif

    htmp = __img__ (hax, do_new, x, y, img, varargin{chararg:end});

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

## Generic image creation.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}.  If you're not using gnuplot 4.2 or later, these
## variables are ignored.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = __img__ (hax, do_new, x, y, img, varargin)

  if (! isempty (img))

    if (isempty (x))
      xdata = [];
    else
      xdata = x([1, end])(:).';  # (:).' is a hack to guarantee row vector
    endif

    if (isempty (y))
      ydata = [];
    else
      ydata = y([1, end])(:).';
    endif

    if (numel (x) > 2 && numel (y) > 2)
      ## Test data for non-linear spacing which is unsupported
      tol = .01;  # 1% tolerance.  FIXME: this value was chosen without thought.
      dx = diff (x);
      dxmean = (max (x) - min (x)) / (numel (x) - 1);
      dx = abs ((abs (dx) - dxmean) / dxmean);
      dy = diff (y);
      dymean = (max (y) - min (y)) / (numel (y) - 1);
      dy = abs ((abs (dy) - dymean) / dymean);
      if (any (dx > tol) || any (dy > tol))
        warning (["image: non-linear X, Y data is ignored.  " ...
                  "IMG will be shown with linear mapping"]);
      endif
    endif

  endif  # ! isempty (img)

  if (do_new && ! ishold (hax))
    ## Set axis properties for new images
    ## NOTE: Do this before calling __go_image__ so that image is not drawn
    ##       once with default auto-scale axis limits and then a second time
    ##       with tight axis limits.
    if (! isempty (img))
      if (isempty (get (hax, "children")))
        axis (hax, "tight");
      endif

      if (ndims (img) == 3)
        if (isinteger (img))
          cls = class (img);
          mn = intmin (cls);
          mx = intmax (cls);
          set (hax, "clim", double ([mn, mx]));
        endif
      endif

    endif  # ! isempty (img)

    set (hax, "view", [0, 90], "ydir", "reverse", "layer", "top");

  endif  # do_new

  h = __go_image__ (hax, "cdata", img, "xdata", xdata, "ydata", ydata,
                         "cdatamapping", "direct", varargin{:});

  if (do_new && ! ishold (hax) && ! isempty (img)
      && isscalar (get (hax, "children")))
    ## Re-scale axis limits for an image in a new figure or axis.
    axis (hax, "tight");
  endif

endfunction


%!demo
%! clf;
%! colormap (jet (21));
%! img = 1 ./ hilb (11);
%! x = y = -5:5;
%! subplot (2,2,1);
%!  h = image (x, y, img);
%!  ylabel ("limits = [-5.5, 5.5]");
%!  title ("image (x, y, img)");
%! subplot (2,2,2);
%!  h = image (-x, y, img);
%!  title ("image (-x, y, img)");
%! subplot (2,2,3);
%!  h = image (x, -y, img);
%!  title ("image (x, -y, img)");
%!  ylabel ("limits = [-5.5, 5.5]");
%! subplot (2,2,4);
%!  h = image (-x, -y, img);
%!  title ("image (-x, -y, img)");

%!test
%! ## test hidden properties x/ydatamode (bug #42121)
%! hf = figure ("visible", "off");
%! unwind_protect
%!   nx = 64; ny = 64;
%!   cdata = rand (ny, nx)*127;
%!   hi = image (cdata);             # x/ydatamode is auto
%!   assert (get (hi, "xdata"), [1 nx])
%!   assert (get (hi, "ydata"), [1 ny])
%!   set (hi, "cdata", cdata(1:2:end, 1:2:end))
%!   assert (get (hi, "xdata"), [1 nx/2])
%!   assert (get (hi, "ydata"), [1 ny/2])
%!
%!   set (hi, "xdata", [10 100])     # xdatamode is now manual
%!   set (hi, "ydata", [10 1000])    # ydatamode is now manual
%!   set (hi, "cdata", cdata)
%!   assert (get (hi, "xdata"), [10 100])
%!   assert (get (hi, "ydata"), [10 1000])
%!
%!   set (hi, "ydata", [])           # ydatamode is now auto
%!   set (hi, "cdata", cdata(1:2:end, 1:2:end))
%!   assert (get (hi, "xdata"), [10 100])
%!   assert (get (hi, "ydata"), [1 ny/2])
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## FIXME: Need %!tests for linear

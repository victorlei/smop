## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} clabel (@var{c}, @var{h})
## @deftypefnx {Function File} {} clabel (@var{c}, @var{h}, @var{v})
## @deftypefnx {Function File} {} clabel (@var{c}, @var{h}, "manual")
## @deftypefnx {Function File} {} clabel (@var{c})
## @deftypefnx {Function File} {} clabel (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} clabel (@dots{})
## Add labels to the contours of a contour plot.
##
## The contour levels are specified by the contour matrix @var{c} which is
## returned by @code{contour}, @code{contourc}, @code{contourf}, and
## @code{contour3}.  Contour labels are rotated to match the local line
## orientation and centered on the line.  The position of labels along the
## contour line is chosen randomly.
##
## If the argument @var{h} is a handle to a contour group object, then label
## this plot rather than the one in the current axes returned by @code{gca}.
##
## By default, all contours are labeled.  However, the contours to label can be
## specified by the vector @var{v}.  If the @qcode{"manual"} argument is
## given then the contours to label can be selected with the mouse.
##
## Additional property/value pairs that are valid properties of text objects
## can be given and are passed to the underlying text objects.  Moreover,
## the contour group property @qcode{"LabelSpacing"} is available which
## determines the spacing between labels on a contour to be specified.  The
## default is 144 points, or 2 inches.
##
## The optional return value @var{h} is a vector of graphics handles to
## the text objects representing each label.
## The @qcode{"userdata"} property of the text objects contains the numerical
## value of the contour label.
##
## An example of the use of @code{clabel} is
##
## @example
## @group
## [c, h] = contour (peaks (), -4 : 6);
## clabel (c, h, -4:2:6, "fontsize", 12);
## @end group
## @end example
##
## @seealso{contour, contourf, contour3, meshc, surfc, text}
## @end deftypefn

function h = clabel (c, varargin)

  have_hg = false;
  have_labelspacing = false;
  label_spacing = 144;  # 2 inches in points

  if (nargin < 1)
    print_usage ();
  elseif (nargin == 1)
    hparent = gca ();
  else
    arg = varargin{1};
    if (isscalar (arg) && ishandle (arg)
        && strcmp (get (arg, "type"), "hggroup"))
      try
        get (arg, "contourmatrix");
      catch
        error ("clabel: H must be a handle to a contour group");
      end_try_catch
      have_hg = true;
      hg = arg;
      varargin(1) = [];
    else
      hparent = gca ();
    endif
  endif

  if (length (varargin) > 0 && isnumeric (varargin{1}))
    v = varargin{1}(:);
    varargin(1) = [];
  else
    v = [];
  endif

  idx = strcmpi (varargin(1:2:end), "manual");
  if (any (idx))
    error ('clabel: "manual" contour mode is not supported');
  endif

  idx = find (strcmpi (varargin(1:2:end), "labelspacing"), 1);
  if (! isempty (idx))
    have_labelspacing = true;
    label_spacing = varargin{2*idx};
    varargin(2*idx+(-1:0)) = [];
  endif

  if (have_hg)
    if (! isempty (v))
      if (have_labelspacing)
        set (hg, "textlistmode", "manual", "textlist", v,
                 "labelspacing", label_spacing, "showtext", "on");
      else
        set (hg, "textlistmode", "manual", "textlist", v, "showtext", "on");
      endif
    else
      if (have_labelspacing)
        set (hg, "showtext", "on", "labelspacing", label_spacing);
      else
        set (hg, "showtext", "on");
      endif
    endif
    htmp = findobj (hg, "type", "text");
    if (! isempty (varargin))
      set (htmp, varargin{:});
    endif
  else
    htmp = __clabel__ (c, v, hparent, label_spacing, [], varargin{:});
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [c, h] = contour (peaks (), -4:6);
%! clabel (c, h, -4:2:6, 'fontsize', 12);
%! title ('clabel() labeling every other contour');

%!demo
%! clf;
%! colormap ('default');
%! [c, h] = contourf (peaks (), -7:6);
%! clabel (c, h, -6:2:6, 'fontsize', 12);
%! title ('clabel() labeling every other contour');


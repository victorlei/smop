## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} orient (@var{orientation})
## @deftypefnx {Function File} {} orient (@var{hfig}, @var{orientation})
## @deftypefnx {Function File} {@var{orientation} =} orient ()
## @deftypefnx {Function File} {@var{orientation} =} orient (@var{hfig})
## Query or set the print orientation for figure @var{hfig}.
##
## Valid values for @var{orientation} are @qcode{"portrait"},
## @qcode{"landscape"}, and @qcode{"tall"}.
##
## The @qcode{"landscape"} option changes the orientation so the plot width
## is larger than the plot height.  The @qcode{"paperposition"} is also
## modified so that the plot fills the page, while leaving a 0.25 inch border.
##
## The @qcode{"tall"} option sets the orientation to @qcode{"portrait"} and
## fills the page with the plot, while leaving a 0.25 inch border.
##
## The @qcode{"portrait"} option (default) changes the orientation so the plot
## height is larger than the plot width.  It also restores the default
## @qcode{"paperposition"} property.
##
## When called with no arguments, return the current print orientation.
##
## If the argument @var{hfig} is omitted, then operate on the current figure
## returned by @code{gcf}.
## @seealso{print, saveas}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-By: jwe

function retval = orient (varargin)

  nargs = nargin;

  if (nargs > 0 && numel (varargin{1}) == 1 && isfigure (varargin{1}))
    cf = varargin{1};
    varargin(1) = [];
    nargs--;
  else
    cf = gcf ();
  endif

  if (nargs > 1)
    print_usage ();
  endif

  paperunits = get (cf, "paperunits");
  unwind_protect
    set (cf, "paperunits", "inches");  # All Matlab calculations assume inches.

    if (nargs == 0)
      retval = get (cf, "paperorientation");
      if (strcmp (retval, "portrait"))
        papersize = get (cf, "papersize");
        paperposition = get (cf, "paperposition");
        if (paperposition == [0.25 0.25 (papersize - 0.5)])
          retval = "tall";
        endif
      endif
    else
      orientation = varargin{1};
      if (strcmpi (orientation, "landscape")
          || strcmpi (orientation, "portrait"))
        if (! strcmpi (get (cf, "paperorientation"), orientation))
          ## FIXME: with the proper listeners in place there won't be a need to
          ##        set the papersize and paperposition here.
          papersize = get (cf, "papersize");
          paperposition = get (cf, "paperposition");
          set (cf, "paperorientation", orientation);
          set (cf, "papersize", papersize([2, 1]));
          set (cf, "paperposition", paperposition([2, 1, 4, 3]));
        endif
        if (strcmpi (orientation, "portrait"))
          ## portrait restores the default
          ## FIXME: Should use "default" here, but Octave complains
          ##        that "paperposition" is not a default property.
          set (cf, "paperposition", "factory");
        else
          ## landscape also sets the plot to occupy the entire page
          papersize = get (cf, "papersize");
          set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
        endif
      elseif (strcmpi (varargin{1}, "tall"))
        orient ("portrait");
        papersize = get (cf, "papersize");
        set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
      else
        error ("orient: unknown ORIENTATION");
      endif
    endif

  unwind_protect_cleanup
    set (cf, "paperunits", paperunits);
  end_unwind_protect

endfunction


%!shared papersize, paperposition, fullpaperposition, hfig
%! papersize = [8.5, 11];
%! paperposition = [0.25, 2.5, 8, 6];
%! fullpaperposition = [0.25, 0.25, (papersize-0.5)];
%! hfig = figure ("visible", "off");
%! set (hfig, "paperunits", "inches");
%! set (hfig, "paperorientation", "portrait");
%! set (hfig, "papersize", papersize);
%! set (hfig, "paperposition", paperposition);

%!test
%! orient portrait;
%! assert (orient, "portrait")   # default
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), paperposition);

%!test
%! orient landscape;
%! assert (orient,"landscape")   # change to landscape
%! assert (get (hfig, "papersize"), papersize([2, 1]));
%! assert (get (hfig, "paperposition"), fullpaperposition([1, 2, 4, 3]));

%!test
%! orient portrait   # change back to portrait
%! assert (orient, "portrait");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), paperposition);

%!test
%! orient landscape;
%! orient tall;
%! assert (orient, "tall");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), fullpaperposition);

%!test
%! orient portrait   # errors don't change the state
%! assert (orient, "portrait");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), paperposition);

## Test input validation
%!error orient (1.73, 2.5)
%!error <unknown ORIENTATION> orient ("nobody")

%!test
%! close (hfig);


## Copyright (C) 2009-2015 Ben Abbott
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
## @deftypefn  {Function File} {} __actual_axis_position__ (@var{h})
## @deftypefnx {Function File} {} __actual_axis_position__ (@var{axis_struct})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott

function pos = __actual_axis_position__ (h)

  if (ishandle (h))
    axis_obj = get (h);
  elseif (isstruct (h))
    axis_obj = h;
    h = axis_obj.__cbar_hax__;
  endif

  ## Get figure size in pixels
  orig_fig_units = get (axis_obj.parent, "units");
  orig_fig_position = get (axis_obj.parent, "position");
  unwind_protect
    set (axis_obj.parent, "units", "pixels");
    fig_position = get (axis_obj.parent, "position");
  unwind_protect_cleanup
    set (axis_obj.parent, "units", orig_fig_units);
    set (axis_obj.parent, "position", orig_fig_position);
  end_unwind_protect
  ## Get axes size in pixels
  if (strcmp (get (axis_obj.parent, "__graphics_toolkit__"), "gnuplot")
      && strcmp (axis_obj.activepositionproperty, "outerposition"))
    pos_in_pixels = axis_obj.outerposition .* fig_position([3, 4, 3, 4]);
  else
    pos_in_pixels = axis_obj.position .* fig_position([3, 4, 3, 4]);
  endif

  nd = __calc_dimensions__ (h);

  if (strcmp (axis_obj.plotboxaspectratiomode, "manual")
      || strcmp (axis_obj.dataaspectratiomode, "manual"))
    ## When using {rltb}margin, Gnuplot does not handle the specified
    ## aspect ratio properly, so handle it here.
    if (nd == 2 || all (mod (axis_obj.view, 90) == 0))
      aspect_ratio_2d = axis_obj.plotboxaspectratio(1:2);
    else
      ## FIXME: This works for "axis square", but has not been
      ##        thoroughly tested for other aspect ratios.
      aspect_ratio_2d = [max(axis_obj.plotboxaspectratio(1:2)), ...
                             axis_obj.plotboxaspectratio(3)/sqrt(2)];
    endif
    orig_aspect_ratio_2d = pos_in_pixels(3:4);
    rel_aspect_ratio_2d = aspect_ratio_2d ./ orig_aspect_ratio_2d;
    rel_aspect_ratio_2d = rel_aspect_ratio_2d ./ max (rel_aspect_ratio_2d);
    if (rel_aspect_ratio_2d(1) < rel_aspect_ratio_2d(2));
      dx = (1.0 - rel_aspect_ratio_2d(1)) * pos_in_pixels(3);
      pos_in_pixels = pos_in_pixels + dx*[0.5, 0.0, -1.0, 0.0];
    elseif (rel_aspect_ratio_2d(1) > rel_aspect_ratio_2d(2))
      dy = (1.0 - rel_aspect_ratio_2d(2)) * pos_in_pixels(4);
      pos_in_pixels = pos_in_pixels + dy*[0.0, 0.5, 0.0, -1.0];
    endif
    pos = pos_in_pixels ./ fig_position([3, 4, 3, 4]);
  elseif (strcmp (get (axis_obj.parent, "__graphics_toolkit__"), "gnuplot")
          && strcmp (axis_obj.activepositionproperty, "outerposition"))
    pos = axis_obj.outerposition;
  else
    pos = axis_obj.position;
  endif
endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)

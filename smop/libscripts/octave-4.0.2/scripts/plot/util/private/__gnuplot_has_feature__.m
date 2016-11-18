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
## @deftypefn {Function File} {@var{has_feature} =} __gnuplot_has_feature__ (@var{feature})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2009-01-27

function res = __gnuplot_has_feature__ (feature)
  persistent features = {"x11_figure_position",
                         "wxt_figure_size",
                         "transparent_patches",
                         "transparent_surface",
                         "epslatex_implies_eps_filesuffix",
                         "epslatexstandalone_terminal",
                         "screen_coordinates_for_{lrtb}margin",
                         "variable_GPVAL_TERMINALS",
                         "key_has_font_properties",
                         "windows_figure_position",
                         "has_termoption_dashed",
                         "needs_color_with_postscript"};
  persistent has_features;

  if (isempty (has_features))
    try
      gnuplot_version = __gnuplot_version__ ();
    catch
      ## Don't throw an error if gnuplot isn't installed
      gnuplot_version = "0.0.0";
    end_try_catch
    versions = {"4.2.5", "4.4", "4.4", "4.4", "4.2", "4.2", "4.4", "4.4", "4.4", "4.4", "4.3", "5.0"};
    operators = {">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">="};
    have_features = false (size (features));
    for n = 1 : numel (have_features)
      has_features(n) = compare_versions (gnuplot_version, versions{n}, operators{n});
    endfor
  endif

  n = find (strcmpi (feature, features));
  if (isempty (n))
    res = NaN;
  else
    res = has_features(n);
  endif

endfunction


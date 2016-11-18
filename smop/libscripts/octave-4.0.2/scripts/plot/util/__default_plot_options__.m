## Copyright (C) 2007-2015 John W. Eaton
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
## @deftypefn {Function File} {@var{options} =} __default_plot_options__ ()
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function options = __default_plot_options__ ()

  options.key = "";
  options.color = [];
  options.linestyle = [];
  options.marker = [];
  options.errorstyle = [];

endfunction


%!test
%! options = __default_plot_options__ ();
%! assert (isfield (options, "key"));
%! assert (options.key, "");
%! assert (options.color, []);
%! assert (options.linestyle, []);
%! assert (options.marker, []);
%! assert (options.errorstyle, []);


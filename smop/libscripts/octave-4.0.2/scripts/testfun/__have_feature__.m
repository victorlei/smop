## Copyright (C) 2013-2015 John W. Eaton
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
## @deftypefn {Function File} {} __have_feature__ (feature)
## Undocumented internal function.
## @end deftypefn

function retval = __have_feature__ (feature)
  features = octave_config_info ("features");
  if (iscellstr (feature))
    retval = (all (isfield (features, feature))
              && cellfun (@(x) features.(x), feature));
  elseif (ischar (feature))
    retval = isfield (features, feature) && features.(feature);
  else
    retval = false;
  endif
endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)


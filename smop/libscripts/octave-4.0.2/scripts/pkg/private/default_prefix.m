## Copyright (C) 2014-2015 Carlo de Falco
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
## @deftypefn {Function File} {[@var{prefix}, @var{archprefix} =} default_prefix (@var{global_install})
## Undocumented internal function.
## @end deftypefn

function [prefix, archprefix] = default_prefix (global_install, desc)
  if (global_install)
    prefix = fullfile (OCTAVE_HOME (), "share", "octave", "packages");
    if (nargin == 2)
      archprefix = fullfile (octave_config_info ("libdir"), "octave",
                             "packages", [desc.name "-" desc.version]);
    else
      archprefix = fullfile (octave_config_info ("libdir"), "octave",
                             "packages");
    endif
  else
    prefix = tilde_expand (fullfile ("~", "octave"));
    archprefix = prefix;
  endif
endfunction


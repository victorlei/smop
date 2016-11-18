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
## @deftypefn {Function File} {} namelengthmax ()
## Return the @sc{matlab} compatible maximum variable name length.
##
## Octave is capable of storing strings up to @math{2^{31} - 1} in length.
## However for @sc{matlab} compatibility all variable, function, and structure
## field names should be shorter than the length returned by
## @code{namelengthmax}.  In particular, variables stored to a @sc{matlab} file
## format (@file{*.mat}) will have their names truncated to this length.
## @end deftypefn

function n = namelengthmax ()
  n = 63;
endfunction


%!assert (namelengthmax (), 63)


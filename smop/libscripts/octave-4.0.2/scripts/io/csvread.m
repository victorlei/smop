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
## @deftypefn  {Function File} {@var{x} =} csvread (@var{filename})
## @deftypefnx {Function File} {@var{x} =} csvread (@var{filename}, @var{dlm_opts})
## Read the comma-separated-value file @var{filename} into the matrix @var{x}.
##
## This function is equivalent to
##
## @example
## @var{x} = dlmread (@var{filename}, "," , @dots{})
## @end example
##
## @seealso{csvwrite, dlmread, dlmwrite}
## @end deftypefn

function x = csvread (filename, varargin)
  x = dlmread (filename, ",", varargin{:});
endfunction


## Tests for csvread() are in csvwrite()
## Mark file as being tested
%!assert (1)


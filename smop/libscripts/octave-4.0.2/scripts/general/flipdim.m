## Copyright (C) 2004-2015 David Bateman
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} flipdim (@var{x})
## @deftypefnx {Function File} {} flipdim (@var{x}, @var{dim})
## Flip array across dimension @var{dim}.
##
## This function is an alias for @code{flip} and exists for backwards and
## @sc{matlab} compatibility.  See @code{flip} for complete usage information.
##
## @seealso{flip, fliplr, flipud, rot90, rotdim}
## @end deftypefn

## Author: David Bateman, Jaroslav Hajek

function y = flipdim (varargin)
  y = flip (varargin{:});
endfunction


## No tests needed for alias.  All tests for functionality are in flip.m
%!assert (1)


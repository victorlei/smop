## Copyright (C) 2005-2015 David Bateman
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
## @deftypefn {Function File} {} __printf_assert__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function __printf_assert__ (varargin)
  global _assert_printf = "";
  _assert_printf = cat (2, _assert_printf, sprintf (varargin{:}));
endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)


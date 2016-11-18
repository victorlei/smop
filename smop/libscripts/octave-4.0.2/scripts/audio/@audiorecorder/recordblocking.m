## Copyright (C) 2013-2015 Vytautas Jančauskas
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
## @deftypefn {Function File} {} recordblocking (@var{recorder}, @var{length})
## Record audio with blocking (synchronous I/O).
##
## The length of the recording in seconds (@var{length}) must be specified.
## @end deftypefn

function recordblocking (varargin)

  if (nargin != 2)
    print_usage ();
  endif

  __recorder_recordblocking__ (struct (varargin{1}).recorder, varargin{2});

endfunction

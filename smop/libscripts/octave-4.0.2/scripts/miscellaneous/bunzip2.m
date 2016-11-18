## Copyright (C) 2006-2015 Bill Denney
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
## @deftypefn  {Function File} {@var{filelist} =} bunzip2 (@var{bzfile})
## @deftypefnx {Function File} {@var{filelist} =} bunzip2 (@var{bzfile}, @var{dir})
## Unpack the bzip2 archive @var{bzfile}.
##
## If @var{dir} is specified the files are unpacked in this directory rather
## than the one where @var{bzfile} is located.
##
## The optional output @var{filelist} is a list of the uncompressed files.
## @seealso{bzip2, unpack, gunzip, unzip, untar}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function filelist = bunzip2 (bzfile, dir = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isempty (dir))
    dir = fileparts (bzfile);
  endif

  if (nargout > 0)
    filelist = unpack (bzfile, dir, "bunzip2");
  else
    unpack (bzfile, dir, "bunzip2");
  endif

endfunction


## Tests for this m-file are located in bzip2.m
## Remove from test statistics
%!assert (1)


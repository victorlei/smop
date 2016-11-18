## Copyright (C) 2005-2015 Søren Hauberg
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
## @deftypefn  {Function File} {@var{filelist} =} unzip (@var{zipfile})
## @deftypefnx {Function File} {@var{filelist} =} unzip (@var{zipfile}, @var{dir})
## Unpack the ZIP archive @var{zipfile}.
##
## If @var{dir} is specified the files are unpacked in this directory rather
## than the one where @var{zipfile} is located.
##
## The optional output @var{filelist} is a list of the uncompressed files.
## @seealso{zip, unpack, bunzip2, gunzip, untar}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe, Bill Denney

function filelist = unzip (zipfile, dir = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isempty (dir))
    dir = fileparts (zipfile);
  endif

  if (nargout > 0)
    filelist = unpack (zipfile, dir, "unzip");
  else
    unpack (zipfile, dir, "unzip");
  endif

endfunction


## Tests for this m-file are located in zip.m
## Remove from test statistics
%!assert (1)

